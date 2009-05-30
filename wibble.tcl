#!/bin/sh
# The next line restarts with tclsh.\
exec tclsh "$0" ${1+"$@"}

package require Tcl 8.6
package provide wibble 0.1

# Define the wibble namespace.
namespace eval wibble {
    namespace export filejoin unhex operation handle listen
    variable zones {}
}

# Echo request dictionary.
proc wibble::vars {request} {
    dict set response status 200
    dict set response header content-type "text/html; charset=utf-8"
    dict set response content {<html><body><table border="1">}
    dict for {key val} $request {
        if {$key in {header query}} {
            set newval ""
            dict for {subkey subval} $val {
                append newval "<b>[list $subkey]</b> [list $subval] "
            }
            set val $newval
        }
        dict append response content <tr><td><b>$key</b></td><td>$val</td></tr>
    }
    dict append response content </table></body></html>\n
    operation sendresponse $response
}

# Redirect when a directory is requested without a trailing slash.
proc wibble::dirslash {request} {
    dict with request {
        if {[file isdirectory $fspath]
         && [string index $suffix end] ni {/ ""}} {
            dict set response status 301
            dict set response header location $path/$querytext
            operation sendresponse $response
        } else {
            operation pass
        }
    }
}

# Rewrite directory requests to search for an indexfile.
proc wibble::indexfile {request} {
    dict with request {
        if {[file isdirectory $fspath]} {
            if {[string index $path end] ne "/"} {
                append path /
            }
            dict set request path $path$indexfile
            operation prependrequest $request
        } else {
            operation pass
        }
    }
}

# Generate directory listings.
proc wibble::dirlist {request} {
    dict with request {
        if {![file isdirectory $fspath]} {
            # Pass if the requested object is not a directory or doesn't exist.
            operation pass
        } elseif {[file readable $fspath]} {
            # If the directory is readable, generate a listing.
            dict set response status 200
            dict set response header content-type "text/html; charset=utf-8"
            dict set response content <html><body>
            foreach elem [concat [list ..]\
            [lsort [glob -nocomplain -tails -directory $fspath *]]] {
                dict append response content\
                    "<a href=\"$elem\">$elem</a><br />"
            }
            dict append response content </body></html>\n
            operation sendresponse $response
        } else {
            # But if it isn't readable, generate a 403.
            dict set response status 403
            dict set response header content-type "text/plain; charset=utf-8"
            dict set response content Forbidden\n
            operation sendresponse $response
        }
    }
}

# Process templates.
proc wibble::template {request} {
    dict with request {
        if {[file readable $fspath.tmpl]} {
            dict set response status 200
            dict set response header content-type "text/plain; charset=utf-8"
            dict set response content ""
            set chan [open $fspath.tmpl]
            applytemplate "dict append response content" [read $chan]
            chan close $chan
            operation sendresponse $response
        } else {
            operation pass
        }
    }
}

# Send static files.
proc wibble::static {request} {
    dict with request {
        if {![file isdirectory $fspath] && [file exists $fspath]} {
            dict set response status 200
            dict set response contentfile $fspath
            operation sendresponse $response
        } else {
            operation pass
        }
    }
}

# Send a 404.
proc wibble::notfound {request} {
    operation sendresponse [dict create status 404\
        content "can't find [dict get $request uri]"\
        header [dict create content-type "text/plain; charset=utf-8"]]
}

# Apply a template.
proc wibble::applytemplate {command template} {
    set script ""
    set pos 0
    foreach pair [regexp -line -all -inline -indices {^%.*$} $template] {
        lassign $pair from to
        set str [string range $template $pos [expr {$from - 2}]]
        if {$str ne ""} {
            append script "$command \[" [list subst $str\n] \]\n
        }
        append script [string range $template [expr {$from + 1}] $to]\n
        set pos [expr {$to + 2}]
    }
    set str [string range $template $pos end]
    if {$str ne ""} {
        append script "$command \[" [list subst $str] \]
    }
    uplevel 1 $script
}

# Get a line or a block of data from a channel.
proc wibble::get {chan {size line}} {
    if {$size eq "line"} {
        # Receive a line of text.
        while {1} {
            if {[chan pending input $chan] > 4096} {
                chan puts stderr "line length greater than 4096"
                chan close $chan
                return -level [info level]
            } elseif {[chan gets $chan line] >= 0} {
                return $line
            } elseif {[chan eof $chan]} {
                chan close $chan
                return -level [info level]
            } else {
                yield
            }
        }
    } else {
        # Receive a block of data.
        while {1} {
            set chunklet [chan read $chan $size]
            set size [expr {$size - [string length $chunklet]}]
            append chunk $chunklet
            if {$size == 0} {
                return $chunk
            } elseif {[chan eof $chan]} {
                chan close $chan
                return -level [info level]
            } else {
                yield
            }
        }
    }
}

# Version of [file join] that doesn't do ~user substitution and ignores leading
# slashes, for all elements except the first.
proc wibble::filejoin {args} {
    for {set i 1} {$i < [llength $args]} {incr i} {
        lset args $i ./[lindex $args $i]
    }
    string map {./ ""} [file join {*}$args]
}

# Decode hexadecimal URL encoding.
proc wibble::unhex {str} {
    set pos 0
    while {[regexp -indices -start $pos {%([[:xdigit:]]{2})} $str range code]} {
        set char [binary format H2 [string range $str {*}$code]]
        set str [string replace $str {*}$range $char]
        set pos [expr {[lindex $range 0] + 1}]
    }
    return $str
}

# Zone handler return operation.
proc wibble::operation {opcode {operand ""}} {
    return -level 2 -opcode $opcode $operand
}

# Register a zone handler.
proc wibble::handle {zone command args} {
    dict lappend wibble::zones $zone [list $command $args]
}

# Get an HTTP request from a client.
proc wibble::getrequest {chan peerhost peerport} {
    # The HTTP header uses CR/LF line breaks.
    chan configure $chan -translation crlf

    # Parse the first line.
    regexp {^\s*(\S*)\s+(\S*)\s+(.*?)\s*$} [get $chan] _ method uri protocol
    regexp {^([^?]*)(\?.*)?$} $uri _ path query
    set path [unhex $path]
    set path [regsub -all {(?:/|^)\.(?=/|$)} $path /]
    while {[regexp -indices {(?:/[^/]*/+|^[^/]*/+|^)\.\.(?=/|$)}\
            $path range]} {
        set path [string replace $path {*}$rng ""]
    }
    set path [regsub -all {//+} /$path /]

    # Start building the request structure.
    set request [dict create socket $chan peerhost $peerhost peerport\
        $peerport method $method uri $uri path $path protocol $protocol\
        header {} query {} querytext $query]

    # Parse the headers.
    set headers {}
    while {1} {
        set header [get $chan]
        if {$header eq ""} {
            break
        }
        if {[regexp {^\s*([^:]*)\s*:\s*(.*)\s*$} $header _ key val]} {
            dict set request header [string tolower $key] $val
        }
    }

    # Parse the query string.
    foreach elem [split [string range $query 1 end] &] {
        regexp {^([^=]*)(?:=(.*))?$} $elem _ key val
        dict set request query [unhex [string map {+ " "} $key]]\
                               [unhex [string map {+ " "} $val]]
    }

    # Get the request body, if there is one.
    if {$method in {POST PUT}} {
        if {[dict exists $request header transfer-encoding]
         && [dict get $request header transfer-encoding] eq "chunked"} {
            # Receive chunked request body.
            set data ""
            while {1} {
                set length [get $chan]
                if {$length == 0} {
                    break
                }
                chan configure $chan -translation binary
                append data [get $chan $length]
                chan configure $chan -translation crlf
            }
        } else {
            # Receive non-chunked request body.
            chan configure $chan -translation binary
            set length [dict get $request header content-length]
            set data [get $chan $length]
            chan configure $chan -translation crlf
        }
        dict set request content $data
    }

    return $request
}

# Get a response from the zone handlers.
proc wibble::getresponse {request} {
    variable zones
    set requests [list $request]

    # Process all zones.
    dict for {prefix handlers} $zones {
        # Process all handlers in this zone.
        foreach handler $handlers {
            lassign $handler command options

            # Try all requests against this handler.
            for {set i 0} {$i < [llength $requests]} {incr i} {
                set request [lindex $requests $i]

                # Skip this request if it's not for the current zone.
                set path [dict get $request path]
                set length [string length $prefix]
                if {[string index $prefix end] eq "/"} {
                    set matchprefix $prefix
                    set matchlength $length
                } else {
                    set matchprefix $prefix/
                    set matchlength [expr {$length + 1}]
                }
                if {$path ne $prefix
                 && ![string equal -length $matchlength $matchprefix $path]} {
                    continue
                }

                # Compile a few extra arguments to pass to the handler.
                set extras [dict create\
                    prefix $prefix suffix [string range $path $length end]]
                if {[dict exists $options root]} {
                    dict set extras fspath [filejoin\
                        [dict get $options root]\
                        [dict get $extras suffix]]
                }

                # Invoke the handler.
                set arguments [dict merge $request $options $extras]
                if {[catch {{*}$command $arguments} operand opcode]} {
                    return -options $opcode $operand
                }

                # Process the handler's result operation.
                switch -- [dict get $opcode -opcode] {
                prependrequest {
                    # Put a new higher-priority request in the list.
                    set operand [dict merge $request $operand]
                    set requests [linsert $requests $i $operand]
                    incr i
                    break
                } replacerequest {
                    # Replace the request.
                    set operand [dict merge $request $operand]
                    set requests [lreplace $requests $i $i $operand]
                    break
                } deleterequest {
                    # Delete the request.
                    set requests [lreplace $requests $i $i]
                    break
                } sendresponse {
                    # A response has been obtained.  Return it.
                    return $operand
                } pass {
                    # Fall through to try next request.
                } default {
                    error "invalid opcode \"[dict get $opcode -opcode]\""
                }}
            }
        }
    }

    # Return 501 as default response.
    return [dict create status 501\
         content "not implemented: [dict get $request uri]"\
        header [dict create content-type "text/plain; charset=utf-8"]]
}

# Main connection processing loop.
proc wibble::process {socket peerhost peerport} {
    try {
        chan configure $socket -blocking 0
        while {1} {
            # Get request from client, then formulate a response to the reqeust.
            set request [getrequest $socket $peerhost $peerport]
            set response [getresponse $request]

            # Get the content size.
            if {[dict exists $response contentfile]} {
                set size [file size [dict get $response contentfile]]
                if {[dict get $request method] ne "HEAD"} {
                    # Open the channel now, to catch errors early.
                    set file [open [dict get $response contentfile]]
                    chan configure $file -translation binary
                }
            } elseif {[dict exists $response content]} {
                dict set response content [encoding convertto iso8859-1\
                        [dict get $response content]]
                set size [string length [dict get $response content]]
            } else {
                set size 0
            }

            # Parse the Range request header if present and valid.
            set begin 0
            set end [expr {$size - 1}]
            if {[dict exists $request header range]
             && [regexp {^bytes=(\d*)-(\d*)$} [dict get $request header range]\
                        _ begin end]
             && [dict get $response status] == 200} {
                dict set response status 206
                if {$begin eq "" || $begin >= $size} {
                    set begin 0
                }
                if {$end eq "" || $end >= $size || $end < $begin} {
                    set end [expr {$size - 1}]
                }
            }

            # Add content-length and content-range response headers.
            set length [expr {$end - $begin + 1}]
            dict set response header content-length $length
            if {[dict get $response status] == 206} {
                dict set response header content-range "bytes $begin-$end/$size"
            }

            # Send the response header to the client.
            chan puts $socket "HTTP/1.1 [dict get $response status]"
            dict for {key val} [dict get $response header] {
                set normalizedkey [lsearch -exact -sorted -inline -nocase {
                    Accept-Ranges Age Allow Cache-Control Connection
                    Content-Disposition Content-Encoding Content-Language
                    Content-Length Content-Location Content-MD5 Content-Range
                    Content-Type Date ETag Expires Last-Modified Location Pragma
                    Proxy-Authenticate Retry-After Server Set-Cookie Trailer
                    Transfer-Encoding Upgrade Vary Via Warning WWW-Authenticate
                } $key]
                if {$normalizedkey ne ""} {
                    chan puts $socket "$normalizedkey: $val"
                } else {
                    chan puts $socket "$key: $val"
                }
            }
            chan puts $socket ""

            # If requested, send the response content to the client.
            if {[dict get $request method] ne "HEAD"} {
                chan configure $socket -translation binary
                if {[dict exists $response contentfile]} {
                    # Send response content from a file.
                    chan seek $file $begin
                    chan copy $file $socket -size $length
                    chan close $file
                } elseif {[dict exists $response content]} {
                    # Send buffered response content.
                    chan puts -nonewline $socket [string range\
                            [dict get $response content] $begin $end]
                }
            }

            # Flush the outgoing buffer.
            chan flush $socket
        }
    } on error {"" options} {
        # Log errors and report them to the client, if possible.
        variable errorcount
        incr errorcount
        set message "*** INTERNAL SERVER ERROR (BEGIN #$errorcount) ***\n"
        append message "time: [clock format [clock seconds]]\n"
        append message "address: $peerhost\n"
        if {[info exists request]} {
            dict for {key val} $request {
                if {$key eq "content" && [string length $val] > 256} {
                    append message "request $key (len=[string length $val])\n"
                } elseif {$key in {header query}} { 
                    dict for {subkey subval} $val {
                        append message "request $key $subkey: $subval\n"
                    }
                } else {
                    append message "request $key: $val\n"
                }
            }
        }
        append message "errorinfo: [dict get $options -errorinfo]\n"
        append message "*** INTERNAL SERVER ERROR (END #$errorcount) ***\n"
        log $message
        try {
            set message [encoding convertto iso8859-1 $message]
            chan configure $socket -translation crlf
            chan puts $socket "HTTP/1.1 500 Internal Server Error"
            chan puts $socket "Content-Type: text/plain; charset=utf-8"
            chan puts $socket "Content-Length: [string length $message]"
            chan puts $socket "Connection: close"
            chan puts $socket ""
            chan configure $socket -translation binary
            chan puts -nonewline $socket $message
        }
    } finally {
        catch {chan close $socket}
    }
}

# Accept an incoming connection.
proc wibble::accept {socket peerhost peerport} {
    chan event $socket readable [namespace code $socket]
    coroutine $socket process $socket $peerhost $peerport
}

# Listen for incoming connections.
proc wibble::listen {port} {
    socket -server [namespace code accept] $port
}

# Log an error.  Feel free to replace this procedure as needed.
proc wibble::log {message} {
    chan puts stderr -nonewline $message
}

# Demonstrate Wibble if being run directly.
if {$argv0 eq [info script]} {
    # Guess the root directory.
    set root [file normalize [file dirname [info script]]]

    # Define zone handlers.
    wibble::handle /vars vars
    wibble::handle / dirslash root $root
    wibble::handle / indexfile root $root indexfile index.html
    wibble::handle / static root $root
    wibble::handle / template root $root
    wibble::handle / dirlist root $root
    wibble::handle / notfound

    # Start a server.
    wibble::listen 8080

    # Enter the event loop.
    vwait forever
}

# vim: set sts=4 sw=4 tw=80 et ft=tcl:
