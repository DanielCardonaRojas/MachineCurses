#!/usr/bin/env bash

#curl -s -X POST -F 'like=turnstat-cruz' 'http://10.1.1.6:5000/support/machines_search' | jq '[.result[] | {vpn: .vpn, hostname: .hostname, office: .office_name, mac: .mac}]' 
#echo "Argument $1"
curl -s -X POST -F "like=$1" "http://10.1.1.6:5000/support/machines_search" | jq "[.result[] | select(.hostname!=null and .mac!=null) | {vpn: .vpn, hostname: .hostname, office: .office_name, mac: .mac}]" 
#curl -s "https://httpbin.org/get"
