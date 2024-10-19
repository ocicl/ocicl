# winhttp
CFFI bindings to WinHttp.dll. Provides a native HTTP client for those few Common Lisp Windows users.

## 1. Introduction
The canonical Common Lisp HTTP client is `drakma` which makes use of `cl+ssl`. This in turn
requires openssl binaries installed on your system. All of this works great on Linux
but on Windows this is generally a very poor experience. 

However, Microsoft ships a fully featured HTTP client (WinHttp.dll) with Windows and is available
on all Windows systems, no installing third party libraries is required.

This project provides a set of cffi bindings to the WinHttp API. 

## Usage

For almost all uses, the `HTTP-REQUEST` function is sufficient. It allows sending
POST data, optional extra headers, basic authentication credentials.

```
(http-request "http://www.google.com")
```

For advanced uses, make use of the `with-http`, `with-connect`, `with-request` macros and
other functions. See the relevant MSDN pages for usage of the underlying APIs.

## Examples

I've included an example XML-RPC client that I have found useful. It is largely taken from the 
S-XML-RPC project, replacing the http client with winhttp.

```
(xmlrpc:call "https://username:password@hostname:port/RPC2" "xmlrpc_method" arg1 arg2 ...)
```

## License

Released under the terms of the MIT license.

Frank James
July 2017.
