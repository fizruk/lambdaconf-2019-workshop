#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

if [[ "$LOCAL_IP_ADDRESS" =~ ^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$ ]]; then
  echo "Generating SSL certificate for IP address $LOCAL_IP_ADDRESS"
  SUBJ_OPTIONS="/C=AU/ST=Some-State/L=/O=AR Cube Demo/OU=/CN=$LOCAL_IP_ADDRESS"
  echo "Certificate options: $SUBJ_OPTIONS"
  openssl genrsa -out key.pem 2048
  openssl req -new -key key.pem -out certificate.csr -subj "$SUBJ_OPTIONS"
else
  echo "Generating SSL certificate..."
  SUBJ_OPTIONS=""
  openssl genrsa -out key.pem 2048
  openssl req -new -key key.pem -out certificate.csr
fi

openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
