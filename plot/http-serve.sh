#!/bin/sh
cd $(dirname $0)
erl -noshell -s inets -eval 'inets:start(httpd,[{server_name,"NAME"},{document_root, "."},{server_root, "."},{port, 8000}])'
