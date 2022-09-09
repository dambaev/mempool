# This file has been generated by node2nix 1.9.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, nix-gitignore, stdenv, lib, globalBuildInputs ? []}:

let
  sources = {
    "@mempool/bitcoin-3.0.3" = {
      name = "_at_mempool_slash_bitcoin";
      packageName = "@mempool/bitcoin";
      version = "3.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/@mempool/bitcoin/-/bitcoin-3.0.3.tgz";
        sha512 = "10UdbwchnevlebDTN+Xhv75AEhDmTMy9UgWHlqx5MG2mheFG6+eqmtHsdxeYnv3IAtTtlRfA6fY0RbV/x4TNFQ==";
      };
    };
    "@mempool/electrum-client-1.1.8" = {
      name = "_at_mempool_slash_electrum-client";
      packageName = "@mempool/electrum-client";
      version = "1.1.8";
      src = fetchurl {
        url = "https://registry.npmjs.org/@mempool/electrum-client/-/electrum-client-1.1.8.tgz";
        sha512 = "6YP6UJstlk2GgC++NwPJthMPvLozyEMlqPq7RjvIWSwrL0smvM0Q0PAOohwZJtJFDWspuEUtNRF7aHQT2ztnYg==";
      };
    };
    "@types/node-14.14.20" = {
      name = "_at_types_slash_node";
      packageName = "@types/node";
      version = "14.14.20";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/node/-/node-14.14.20.tgz";
        sha512 = "Y93R97Ouif9JEOWPIUyU+eyIdyRqQR0I8Ez1dzku4hDx34NWh4HbtIc3WNzwB1Y9ULvNGeu5B8h8bVL5cAk4/A==";
      };
    };
    "@types/ws-8.2.2" = {
      name = "_at_types_slash_ws";
      packageName = "@types/ws";
      version = "8.2.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/ws/-/ws-8.2.2.tgz";
        sha512 = "NOn5eIcgWLOo6qW8AcuLZ7G8PycXu0xTxxkS6Q18VWFxgPUSOwV0pBj2a/4viNZVu25i7RIB7GttdkAIUUXOOg==";
      };
    };
    "accepts-1.3.7" = {
      name = "accepts";
      packageName = "accepts";
      version = "1.3.7";
      src = fetchurl {
        url = "https://registry.npmjs.org/accepts/-/accepts-1.3.7.tgz";
        sha512 = "Il80Qs2WjYlJIBNzNkK6KYqlVMTbZLXgHx2oT0pU/fjRHyEp+PEfEPY0R3WCwAGVOtauxh1hOxNgIf5bv7dQpA==";
      };
    };
    "array-flatten-1.1.1" = {
      name = "array-flatten";
      packageName = "array-flatten";
      version = "1.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/array-flatten/-/array-flatten-1.1.1.tgz";
        sha1 = "9a5f699051b1e7073328f2a008968b64ea2955d2";
      };
    };
    "axios-0.24.0" = {
      name = "axios";
      packageName = "axios";
      version = "0.24.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/axios/-/axios-0.24.0.tgz";
        sha512 = "Q6cWsys88HoPgAaFAVUb0WpPk0O8iTeisR9IMqy9G8AbO4NlpVknrnQS03zzF9PGAWgO3cgletO3VjV/P7VztA==";
      };
    };
    "base-x-3.0.9" = {
      name = "base-x";
      packageName = "base-x";
      version = "3.0.9";
      src = fetchurl {
        url = "https://registry.npmjs.org/base-x/-/base-x-3.0.9.tgz";
        sha512 = "H7JU6iBHTal1gp56aKoaa//YUxEaAOUiydvrV/pILqIHXTtqxSkATOnDA2u+jZ/61sD+L/412+7kzXRtWukhpQ==";
      };
    };
    "bech32-2.0.0" = {
      name = "bech32";
      packageName = "bech32";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/bech32/-/bech32-2.0.0.tgz";
        sha512 = "LcknSilhIGatDAsY1ak2I8VtGaHNhgMSYVxFrGLXv+xLHytaKZKcaUJJUE7qmBr7h33o5YQwP55pMI0xmkpJwg==";
      };
    };
    "bip174-2.0.1" = {
      name = "bip174";
      packageName = "bip174";
      version = "2.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/bip174/-/bip174-2.0.1.tgz";
        sha512 = "i3X26uKJOkDTAalYAp0Er+qGMDhrbbh2o93/xiPyAN2s25KrClSpe3VXo/7mNJoqA5qfko8rLS2l3RWZgYmjKQ==";
      };
    };
    "bitcoinjs-lib-6.0.1" = {
      name = "bitcoinjs-lib";
      packageName = "bitcoinjs-lib";
      version = "6.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/bitcoinjs-lib/-/bitcoinjs-lib-6.0.1.tgz";
        sha512 = "x/7D4jDj/MMkmO6t3p2CSDXTqpwZ/jRsRiJDmaiXabrR9XRo7jwby8HRn7EyK1h24rKFFI7vI0ay4czl6bDOZQ==";
      };
    };
    "body-parser-1.19.0" = {
      name = "body-parser";
      packageName = "body-parser";
      version = "1.19.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/body-parser/-/body-parser-1.19.0.tgz";
        sha512 = "dhEPs72UPbDnAQJ9ZKMNTP6ptJaionhP5cBb541nXPlW60Jepo9RV/a4fX4XWW9CuFNK22krhrj1+rgzifNCsw==";
      };
    };
    "bs58-4.0.1" = {
      name = "bs58";
      packageName = "bs58";
      version = "4.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/bs58/-/bs58-4.0.1.tgz";
        sha1 = "be161e76c354f6f788ae4071f63f34e8c4f0a42a";
      };
    };
    "bs58check-2.1.2" = {
      name = "bs58check";
      packageName = "bs58check";
      version = "2.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/bs58check/-/bs58check-2.1.2.tgz";
        sha512 = "0TS1jicxdU09dwJMNZtVAfzPi6Q6QeN0pM1Fkzrjn+XYHvzMKPU3pHVpva+769iNVSfIYWf7LJ6WR+BuuMf8cA==";
      };
    };
    "bytes-3.1.0" = {
      name = "bytes";
      packageName = "bytes";
      version = "3.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/bytes/-/bytes-3.1.0.tgz";
        sha512 = "zauLjrfCG+xvoyaqLoV8bLVXXNGC4JqlxFCutSDWA6fJrTo2ZuvLYTqZ7aHBLZSMOopbzwv8f+wZcVzfVTI2Dg==";
      };
    };
    "cipher-base-1.0.4" = {
      name = "cipher-base";
      packageName = "cipher-base";
      version = "1.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/cipher-base/-/cipher-base-1.0.4.tgz";
        sha512 = "Kkht5ye6ZGmwv40uUDZztayT2ThLQGfnj/T71N/XzeZeo3nf8foyW7zGTsPYkEya3m5f3cAypH+qe7YOrM1U2Q==";
      };
    };
    "content-disposition-0.5.3" = {
      name = "content-disposition";
      packageName = "content-disposition";
      version = "0.5.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/content-disposition/-/content-disposition-0.5.3.tgz";
        sha512 = "ExO0774ikEObIAEV9kDo50o+79VCUdEB6n6lzKgGwupcVeRlhrj3qGAfwq8G6uBJjkqLrhT0qEYFcWng8z1z0g==";
      };
    };
    "content-type-1.0.4" = {
      name = "content-type";
      packageName = "content-type";
      version = "1.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/content-type/-/content-type-1.0.4.tgz";
        sha512 = "hIP3EEPs8tB9AT1L+NUqtwOAps4mk2Zob89MWXMHjHWg9milF/j4osnnQLXBCBFBk/tvIG/tUc9mOUJiPBhPXA==";
      };
    };
    "cookie-0.4.0" = {
      name = "cookie";
      packageName = "cookie";
      version = "0.4.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/cookie/-/cookie-0.4.0.tgz";
        sha512 = "+Hp8fLp57wnUSt0tY0tHEXh4voZRDnoIrZPqlo3DPiI4y9lwg/jqx+1Om94/W6ZaPDOUbnjOt/99w66zk+l1Xg==";
      };
    };
    "cookie-signature-1.0.6" = {
      name = "cookie-signature";
      packageName = "cookie-signature";
      version = "1.0.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/cookie-signature/-/cookie-signature-1.0.6.tgz";
        sha1 = "e303a882b342cc3ee8ca513a79999734dab3ae2c";
      };
    };
    "create-hash-1.2.0" = {
      name = "create-hash";
      packageName = "create-hash";
      version = "1.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/create-hash/-/create-hash-1.2.0.tgz";
        sha512 = "z00bCGNHDG8mHAkP7CtT1qVu+bFQUPjYq/4Iv3C3kWjTFV10zIjfSoeqXo9Asws8gwSHDGj/hl2u4OGIjapeCg==";
      };
    };
    "crypto-js-4.0.0" = {
      name = "crypto-js";
      packageName = "crypto-js";
      version = "4.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/crypto-js/-/crypto-js-4.0.0.tgz";
        sha512 = "bzHZN8Pn+gS7DQA6n+iUmBfl0hO5DJq++QP3U6uTucDtk/0iGpXd/Gg7CGR0p8tJhofJyaKoWBuJI4eAO00BBg==";
      };
    };
    "debug-2.6.9" = {
      name = "debug";
      packageName = "debug";
      version = "2.6.9";
      src = fetchurl {
        url = "https://registry.npmjs.org/debug/-/debug-2.6.9.tgz";
        sha512 = "bC7ElrdJaJnPbAP+1EotYvqZsb3ecl5wi6Bfi6BJTUcNowp6cvspg0jXznRTKDjm/E7AdgFBVeAPVMNcKGsHMA==";
      };
    };
    "denque-2.0.1" = {
      name = "denque";
      packageName = "denque";
      version = "2.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/denque/-/denque-2.0.1.tgz";
        sha512 = "tfiWc6BQLXNLpNiR5iGd0Ocu3P3VpxfzFiqubLgMfhfOw9WyvgJBd46CClNn9k3qfbjvT//0cf7AlYRX/OslMQ==";
      };
    };
    "depd-1.1.2" = {
      name = "depd";
      packageName = "depd";
      version = "1.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/depd/-/depd-1.1.2.tgz";
        sha1 = "9bcd52e14c097763e749b274c4346ed2e560b5a9";
      };
    };
    "destroy-1.0.4" = {
      name = "destroy";
      packageName = "destroy";
      version = "1.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/destroy/-/destroy-1.0.4.tgz";
        sha1 = "978857442c44749e4206613e37946205826abd80";
      };
    };
    "ee-first-1.1.1" = {
      name = "ee-first";
      packageName = "ee-first";
      version = "1.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/ee-first/-/ee-first-1.1.1.tgz";
        sha1 = "590c61156b0ae2f4f0255732a158b266bc56b21d";
      };
    };
    "encodeurl-1.0.2" = {
      name = "encodeurl";
      packageName = "encodeurl";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/encodeurl/-/encodeurl-1.0.2.tgz";
        sha1 = "ad3ff4c86ec2d029322f5a02c3a9a606c95b3f59";
      };
    };
    "escape-html-1.0.3" = {
      name = "escape-html";
      packageName = "escape-html";
      version = "1.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/escape-html/-/escape-html-1.0.3.tgz";
        sha1 = "0258eae4d3d0c0974de1c169188ef0051d1d1988";
      };
    };
    "etag-1.8.1" = {
      name = "etag";
      packageName = "etag";
      version = "1.8.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/etag/-/etag-1.8.1.tgz";
        sha1 = "41ae2eeb65efa62268aebfea83ac7d79299b0887";
      };
    };
    "express-4.17.1" = {
      name = "express";
      packageName = "express";
      version = "4.17.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/express/-/express-4.17.1.tgz";
        sha512 = "mHJ9O79RqluphRrcw2X/GTh3k9tVv8YcoyY4Kkh4WDMUYKRZUq0h1o0w2rrrxBqM7VoeUVqgb27xlEMXTnYt4g==";
      };
    };
    "finalhandler-1.1.2" = {
      name = "finalhandler";
      packageName = "finalhandler";
      version = "1.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/finalhandler/-/finalhandler-1.1.2.tgz";
        sha512 = "aAWcW57uxVNrQZqFXjITpW3sIUQmHGG3qSb9mUah9MgMC4NeWhNOlNjXEYq3HjRAvL6arUviZGGJsBg6z0zsWA==";
      };
    };
    "follow-redirects-1.14.7" = {
      name = "follow-redirects";
      packageName = "follow-redirects";
      version = "1.14.7";
      src = fetchurl {
        url = "https://registry.npmjs.org/follow-redirects/-/follow-redirects-1.14.7.tgz";
        sha512 = "+hbxoLbFMbRKDwohX8GkTataGqO6Jb7jGwpAlwgy2bIz25XtRm7KEzJM76R1WiNT5SwZkX4Y75SwBolkpmE7iQ==";
      };
    };
    "forwarded-0.1.2" = {
      name = "forwarded";
      packageName = "forwarded";
      version = "0.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/forwarded/-/forwarded-0.1.2.tgz";
        sha1 = "98c23dab1175657b8c0573e8ceccd91b0ff18c84";
      };
    };
    "fresh-0.5.2" = {
      name = "fresh";
      packageName = "fresh";
      version = "0.5.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/fresh/-/fresh-0.5.2.tgz";
        sha1 = "3d8cadd90d976569fa835ab1f8e4b23a105605a7";
      };
    };
    "generate-function-2.3.1" = {
      name = "generate-function";
      packageName = "generate-function";
      version = "2.3.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/generate-function/-/generate-function-2.3.1.tgz";
        sha512 = "eeB5GfMNeevm/GRYq20ShmsaGcmI81kIX2K9XQx5miC8KdHaC6Jm0qQ8ZNeGOi7wYB8OsdxKs+Y2oVuTFuVwKQ==";
      };
    };
    "hash-base-3.1.0" = {
      name = "hash-base";
      packageName = "hash-base";
      version = "3.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/hash-base/-/hash-base-3.1.0.tgz";
        sha512 = "1nmYp/rhMDiE7AYkDw+lLwlAzz0AntGIe51F3RfFfEqyQ3feY2eI/NcwC6umIQVOASPMsWJLJScWKSSvzL9IVA==";
      };
    };
    "http-errors-1.7.2" = {
      name = "http-errors";
      packageName = "http-errors";
      version = "1.7.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/http-errors/-/http-errors-1.7.2.tgz";
        sha512 = "uUQBt3H/cSIVfch6i1EuPNy/YsRSOUBXTVfZ+yR7Zjez3qjBz6i9+i4zjNaoqcoFVI4lQJ5plg63TvGfRSDCRg==";
      };
    };
    "iconv-lite-0.4.24" = {
      name = "iconv-lite";
      packageName = "iconv-lite";
      version = "0.4.24";
      src = fetchurl {
        url = "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.4.24.tgz";
        sha512 = "v3MXnZAcvnywkTUEZomIActle7RXXeedOR31wwl7VlyoXO4Qi9arvSenNQWne1TcRwhCL1HwLI21bEqdpj8/rA==";
      };
    };
    "iconv-lite-0.6.3" = {
      name = "iconv-lite";
      packageName = "iconv-lite";
      version = "0.6.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.6.3.tgz";
        sha512 = "4fCk79wshMdzMp2rH06qWrJE4iolqLhCUH+OiuIgU++RB0+94NlDL81atO7GX55uUKueo0txHNtvEyI6D7WdMw==";
      };
    };
    "inherits-2.0.3" = {
      name = "inherits";
      packageName = "inherits";
      version = "2.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/inherits/-/inherits-2.0.3.tgz";
        sha1 = "633c2c83e3da42a502f52466022480f4208261de";
      };
    };
    "inherits-2.0.4" = {
      name = "inherits";
      packageName = "inherits";
      version = "2.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz";
        sha512 = "k/vGaX4/Yla3WzyMCvTQOXYeIHvqOKtnqBduzTHpzpQZzAskKMhZ2K+EnBiSM9zGSoIFeMpXKxa4dYeZIQqewQ==";
      };
    };
    "ipaddr.js-1.9.1" = {
      name = "ipaddr.js";
      packageName = "ipaddr.js";
      version = "1.9.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/ipaddr.js/-/ipaddr.js-1.9.1.tgz";
        sha512 = "0KI/607xoxSToH7GjN1FfSbLoU0+btTicjsQSWQlh/hZykN8KpmMf7uYwPW3R+akZ6R/w18ZlXSHBYXiYUPO3g==";
      };
    };
    "is-property-1.0.2" = {
      name = "is-property";
      packageName = "is-property";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/is-property/-/is-property-1.0.2.tgz";
        sha1 = "57fe1c4e48474edd65b09911f26b1cd4095dda84";
      };
    };
    "locutus-2.0.15" = {
      name = "locutus";
      packageName = "locutus";
      version = "2.0.15";
      src = fetchurl {
        url = "https://registry.npmjs.org/locutus/-/locutus-2.0.15.tgz";
        sha512 = "2xWC4RkoAoCVXEb/stzEgG1TNgd+mrkLBj6TuEDNyUoKeQ2XzDTyJUC23sMiqbL6zJmJSP3w59OZo+zc4IBOmA==";
      };
    };
    "long-4.0.0" = {
      name = "long";
      packageName = "long";
      version = "4.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/long/-/long-4.0.0.tgz";
        sha512 = "XsP+KhQif4bjX1kbuSiySJFNAehNxgLb6hPRGJ9QsUr8ajHkuXGdrHmFUTUUXhDwVX2R5bY4JNZEwbUiMhV+MA==";
      };
    };
    "lru-cache-4.1.5" = {
      name = "lru-cache";
      packageName = "lru-cache";
      version = "4.1.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/lru-cache/-/lru-cache-4.1.5.tgz";
        sha512 = "sWZlbEP2OsHNkXrMl5GYk/jKk70MBng6UU4YI/qGDYbgf6YbP4EvmqISbXCoJiRKs+1bSpFHVgQxvJ17F2li5g==";
      };
    };
    "lru-cache-6.0.0" = {
      name = "lru-cache";
      packageName = "lru-cache";
      version = "6.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/lru-cache/-/lru-cache-6.0.0.tgz";
        sha512 = "Jo6dJ04CmSjuznwJSS3pUeWmd/H0ffTlkXXgwZi+eq1UCmqQwCh+eLsYOYCwY991i2Fah4h1BEMCx4qThGbsiA==";
      };
    };
    "md5.js-1.3.5" = {
      name = "md5.js";
      packageName = "md5.js";
      version = "1.3.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/md5.js/-/md5.js-1.3.5.tgz";
        sha512 = "xitP+WxNPcTTOgnTJcrhM0xvdPepipPSf3I8EIpGKeFLjt3PlJLIDG3u8EX53ZIubkb+5U2+3rELYpEhHhzdkg==";
      };
    };
    "media-typer-0.3.0" = {
      name = "media-typer";
      packageName = "media-typer";
      version = "0.3.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/media-typer/-/media-typer-0.3.0.tgz";
        sha1 = "8710d7af0aa626f8fffa1ce00168545263255748";
      };
    };
    "merge-descriptors-1.0.1" = {
      name = "merge-descriptors";
      packageName = "merge-descriptors";
      version = "1.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/merge-descriptors/-/merge-descriptors-1.0.1.tgz";
        sha1 = "b00aaa556dd8b44568150ec9d1b953f3f90cbb61";
      };
    };
    "methods-1.1.2" = {
      name = "methods";
      packageName = "methods";
      version = "1.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/methods/-/methods-1.1.2.tgz";
        sha1 = "5529a4d67654134edcc5266656835b0f851afcee";
      };
    };
    "mime-1.6.0" = {
      name = "mime";
      packageName = "mime";
      version = "1.6.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/mime/-/mime-1.6.0.tgz";
        sha512 = "x0Vn8spI+wuJ1O6S7gnbaQg8Pxh4NNHb7KSINmEWKiPE4RKOplvijn+NkmYmmRgP68mc70j2EbeTFRsrswaQeg==";
      };
    };
    "mime-db-1.45.0" = {
      name = "mime-db";
      packageName = "mime-db";
      version = "1.45.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/mime-db/-/mime-db-1.45.0.tgz";
        sha512 = "CkqLUxUk15hofLoLyljJSrukZi8mAtgd+yE5uO4tqRZsdsAJKv0O+rFMhVDRJgozy+yG6md5KwuXhD4ocIoP+w==";
      };
    };
    "mime-types-2.1.28" = {
      name = "mime-types";
      packageName = "mime-types";
      version = "2.1.28";
      src = fetchurl {
        url = "https://registry.npmjs.org/mime-types/-/mime-types-2.1.28.tgz";
        sha512 = "0TO2yJ5YHYr7M2zzT7gDU1tbwHxEUWBCLt0lscSNpcdAfFyJOVEpRYNS7EXVcTLNj/25QO8gulHC5JtTzSE2UQ==";
      };
    };
    "ms-2.0.0" = {
      name = "ms";
      packageName = "ms";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/ms/-/ms-2.0.0.tgz";
        sha1 = "5608aeadfc00be6c2901df5f9861788de0d597c8";
      };
    };
    "ms-2.1.1" = {
      name = "ms";
      packageName = "ms";
      version = "2.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/ms/-/ms-2.1.1.tgz";
        sha512 = "tgp+dl5cGk28utYktBsrFqA7HKgrhgPsg6Z/EfhWI4gl1Hwq8B/GmY/0oXZ6nF8hDVesS/FpnYaD/kOWhYQvyg==";
      };
    };
    "mysql2-2.3.3" = {
      name = "mysql2";
      packageName = "mysql2";
      version = "2.3.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/mysql2/-/mysql2-2.3.3.tgz";
        sha512 = "wxJUev6LgMSgACDkb/InIFxDprRa6T95+VEoR+xPvtngtccNH2dGjEB/fVZ8yg1gWv1510c9CvXuJHi5zUm0ZA==";
      };
    };
    "named-placeholders-1.1.2" = {
      name = "named-placeholders";
      packageName = "named-placeholders";
      version = "1.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/named-placeholders/-/named-placeholders-1.1.2.tgz";
        sha512 = "wiFWqxoLL3PGVReSZpjLVxyJ1bRqe+KKJVbr4hGs1KWfTZTQyezHFBbuKj9hsizHyGV2ne7EMjHdxEGAybD5SA==";
      };
    };
    "negotiator-0.6.2" = {
      name = "negotiator";
      packageName = "negotiator";
      version = "0.6.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/negotiator/-/negotiator-0.6.2.tgz";
        sha512 = "hZXc7K2e+PgeI1eDBe/10Ard4ekbfrrqG8Ep+8Jmf4JID2bNg7NvCPOZN+kfF574pFQI7mum2AUqDidoKqcTOw==";
      };
    };
    "node-worker-threads-pool-1.4.3" = {
      name = "node-worker-threads-pool";
      packageName = "node-worker-threads-pool";
      version = "1.4.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/node-worker-threads-pool/-/node-worker-threads-pool-1.4.3.tgz";
        sha512 = "US55ZGzEDQY2oq8Bc33dFVNKGpx4KaCJqThMDomSsUeX8tMdp2eDjQ6OP0yFd1HTEuHuLqxXSTWC4eidEsbXlg==";
      };
    };
    "on-finished-2.3.0" = {
      name = "on-finished";
      packageName = "on-finished";
      version = "2.3.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/on-finished/-/on-finished-2.3.0.tgz";
        sha1 = "20f1336481b083cd75337992a16971aa2d906947";
      };
    };
    "parseurl-1.3.3" = {
      name = "parseurl";
      packageName = "parseurl";
      version = "1.3.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/parseurl/-/parseurl-1.3.3.tgz";
        sha512 = "CiyeOxFT/JZyN5m0z9PfXw4SCBJ6Sygz1Dpl0wqjlhDEGGBP1GnsUVEL0p63hoG1fcj3fHynXi9NYO4nWOL+qQ==";
      };
    };
    "path-to-regexp-0.1.7" = {
      name = "path-to-regexp";
      packageName = "path-to-regexp";
      version = "0.1.7";
      src = fetchurl {
        url = "https://registry.npmjs.org/path-to-regexp/-/path-to-regexp-0.1.7.tgz";
        sha1 = "df604178005f522f15eb4490e7247a1bfaa67f8c";
      };
    };
    "proxy-addr-2.0.6" = {
      name = "proxy-addr";
      packageName = "proxy-addr";
      version = "2.0.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/proxy-addr/-/proxy-addr-2.0.6.tgz";
        sha512 = "dh/frvCBVmSsDYzw6n926jv974gddhkFPfiN8hPOi30Wax25QZyZEGveluCgliBnqmuM+UJmBErbAUFIoDbjOw==";
      };
    };
    "pseudomap-1.0.2" = {
      name = "pseudomap";
      packageName = "pseudomap";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/pseudomap/-/pseudomap-1.0.2.tgz";
        sha1 = "f052a28da70e618917ef0a8ac34c1ae5a68286b3";
      };
    };
    "qs-6.7.0" = {
      name = "qs";
      packageName = "qs";
      version = "6.7.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/qs/-/qs-6.7.0.tgz";
        sha512 = "VCdBRNFTX1fyE7Nb6FYoURo/SPe62QCaAyzJvUjwRaIsc+NePBEniHlvxFmmX56+HZphIGtV0XeCirBtpDrTyQ==";
      };
    };
    "range-parser-1.2.1" = {
      name = "range-parser";
      packageName = "range-parser";
      version = "1.2.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/range-parser/-/range-parser-1.2.1.tgz";
        sha512 = "Hrgsx+orqoygnmhFbKaHE6c296J+HTAQXoxEF6gNupROmmGJRoyzfG3ccAveqCBrwr/2yxQ5BVd/GTl5agOwSg==";
      };
    };
    "raw-body-2.4.0" = {
      name = "raw-body";
      packageName = "raw-body";
      version = "2.4.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/raw-body/-/raw-body-2.4.0.tgz";
        sha512 = "4Oz8DUIwdvoa5qMJelxipzi/iJIi40O5cGV1wNYp5hvZP8ZN0T+jiNkL0QepXs+EsQ9XJ8ipEDoiH70ySUJP3Q==";
      };
    };
    "readable-stream-3.6.0" = {
      name = "readable-stream";
      packageName = "readable-stream";
      version = "3.6.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/readable-stream/-/readable-stream-3.6.0.tgz";
        sha512 = "BViHy7LKeTz4oNnkcLJ+lVSL6vpiFeX6/d3oSH8zCW7UxP2onchk+vTGB143xuFjHS3deTgkKoXXymXqymiIdA==";
      };
    };
    "ripemd160-2.0.2" = {
      name = "ripemd160";
      packageName = "ripemd160";
      version = "2.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/ripemd160/-/ripemd160-2.0.2.tgz";
        sha512 = "ii4iagi25WusVoiC4B4lq7pbXfAp3D9v5CwfkY33vffw2+pkDjY1D8GaN7spsxvCSx8dkPqOZCEZyfxcmJG2IA==";
      };
    };
    "safe-buffer-5.1.2" = {
      name = "safe-buffer";
      packageName = "safe-buffer";
      version = "5.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.1.2.tgz";
        sha512 = "Gd2UZBJDkXlY7GbJxfsE8/nvKkUEU1G38c1siN6QP6a9PT9MmHB8GnpscSmMJSoF8LOIrt8ud/wPtojys4G6+g==";
      };
    };
    "safe-buffer-5.2.1" = {
      name = "safe-buffer";
      packageName = "safe-buffer";
      version = "5.2.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.2.1.tgz";
        sha512 = "rp3So07KcdmmKbGvgaNxQSJr7bGVSVk5S9Eq1F+ppbRo70+YeaDxkw5Dd8NPN+GD6bjnYm2VuPuCXmpuYvmCXQ==";
      };
    };
    "safer-buffer-2.1.2" = {
      name = "safer-buffer";
      packageName = "safer-buffer";
      version = "2.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/safer-buffer/-/safer-buffer-2.1.2.tgz";
        sha512 = "YZo3K82SD7Riyi0E1EQPojLz7kpepnSQI9IyPbHHg1XXXevb5dJI7tpyN2ADxGcQbHG7vcyRHk0cbwqcQriUtg==";
      };
    };
    "send-0.17.1" = {
      name = "send";
      packageName = "send";
      version = "0.17.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/send/-/send-0.17.1.tgz";
        sha512 = "BsVKsiGcQMFwT8UxypobUKyv7irCNRHk1T0G680vk88yf6LBByGcZJOTJCrTP2xVN6yI+XjPJcNuE3V4fT9sAg==";
      };
    };
    "seq-queue-0.0.5" = {
      name = "seq-queue";
      packageName = "seq-queue";
      version = "0.0.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/seq-queue/-/seq-queue-0.0.5.tgz";
        sha1 = "d56812e1c017a6e4e7c3e3a37a1da6d78dd3c93e";
      };
    };
    "serve-static-1.14.1" = {
      name = "serve-static";
      packageName = "serve-static";
      version = "1.14.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/serve-static/-/serve-static-1.14.1.tgz";
        sha512 = "JMrvUwE54emCYWlTI+hGrGv5I8dEwmco/00EvkzIIsR7MqrHonbD9pO2MOfFnpFntl7ecpZs+3mW+XbQZu9QCg==";
      };
    };
    "setprototypeof-1.1.1" = {
      name = "setprototypeof";
      packageName = "setprototypeof";
      version = "1.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/setprototypeof/-/setprototypeof-1.1.1.tgz";
        sha512 = "JvdAWfbXeIGaZ9cILp38HntZSFSo3mWg6xGcJJsd+d4aRMOqauag1C63dJfDw7OaMYwEbHMOxEZ1lqVRYP2OAw==";
      };
    };
    "sha.js-2.4.11" = {
      name = "sha.js";
      packageName = "sha.js";
      version = "2.4.11";
      src = fetchurl {
        url = "https://registry.npmjs.org/sha.js/-/sha.js-2.4.11.tgz";
        sha512 = "QMEp5B7cftE7APOjk5Y6xgrbWu+WkLVQwk8JNjZ8nKRciZaByEW6MubieAiToS7+dwvrjGhH8jRXz3MVd0AYqQ==";
      };
    };
    "sqlstring-2.3.2" = {
      name = "sqlstring";
      packageName = "sqlstring";
      version = "2.3.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/sqlstring/-/sqlstring-2.3.2.tgz";
        sha512 = "vF4ZbYdKS8OnoJAWBmMxCQDkiEBkGQYU7UZPtL8flbDRSNkhaXvRJ279ZtI6M+zDaQovVU4tuRgzK5fVhvFAhg==";
      };
    };
    "statuses-1.5.0" = {
      name = "statuses";
      packageName = "statuses";
      version = "1.5.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/statuses/-/statuses-1.5.0.tgz";
        sha1 = "161c7dac177659fd9811f43771fa99381478628c";
      };
    };
    "string_decoder-1.3.0" = {
      name = "string_decoder";
      packageName = "string_decoder";
      version = "1.3.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/string_decoder/-/string_decoder-1.3.0.tgz";
        sha512 = "hkRX8U1WjJFd8LsDJ2yQ/wWWxaopEsABU1XfkM8A+j0+85JAGppt16cr1Whg6KIbb4okU6Mql6BOj+uup/wKeA==";
      };
    };
    "toidentifier-1.0.0" = {
      name = "toidentifier";
      packageName = "toidentifier";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/toidentifier/-/toidentifier-1.0.0.tgz";
        sha512 = "yaOH/Pk/VEhBWWTlhI+qXxDFXlejDGcQipMlyxda9nthulaxLZUNcUqFxokp0vcYnvteJln5FNQDRrxj3YcbVw==";
      };
    };
    "type-is-1.6.18" = {
      name = "type-is";
      packageName = "type-is";
      version = "1.6.18";
      src = fetchurl {
        url = "https://registry.npmjs.org/type-is/-/type-is-1.6.18.tgz";
        sha512 = "TkRKr9sUTxEH8MdfuCSP7VizJyzRNMjj2J2do2Jr3Kym598JVdEksuzPQCnlFPW4ky9Q+iA+ma9BGm06XQBy8g==";
      };
    };
    "typeforce-1.18.0" = {
      name = "typeforce";
      packageName = "typeforce";
      version = "1.18.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/typeforce/-/typeforce-1.18.0.tgz";
        sha512 = "7uc1O8h1M1g0rArakJdf0uLRSSgFcYexrVoKo+bzJd32gd4gDy2L/Z+8/FjPnU9ydY3pEnVPtr9FyscYY60K1g==";
      };
    };
    "typescript-4.4.4" = {
      name = "typescript";
      packageName = "typescript";
      version = "4.4.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/typescript/-/typescript-4.4.4.tgz";
        sha512 = "DqGhF5IKoBl8WNf8C1gu8q0xZSInh9j1kJJMqT3a94w1JzVaBU4EXOSMrz9yDqMT0xt3selp83fuFMQ0uzv6qA==";
      };
    };
    "unpipe-1.0.0" = {
      name = "unpipe";
      packageName = "unpipe";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/unpipe/-/unpipe-1.0.0.tgz";
        sha1 = "b2bf4ee8514aae6165b4817829d21b2ef49904ec";
      };
    };
    "util-deprecate-1.0.2" = {
      name = "util-deprecate";
      packageName = "util-deprecate";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/util-deprecate/-/util-deprecate-1.0.2.tgz";
        sha1 = "450d4dc9fa70de732762fbd2d4a28981419a0ccf";
      };
    };
    "utils-merge-1.0.1" = {
      name = "utils-merge";
      packageName = "utils-merge";
      version = "1.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/utils-merge/-/utils-merge-1.0.1.tgz";
        sha1 = "9f95710f50a267947b2ccc124741c1028427e713";
      };
    };
    "varuint-bitcoin-1.1.2" = {
      name = "varuint-bitcoin";
      packageName = "varuint-bitcoin";
      version = "1.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/varuint-bitcoin/-/varuint-bitcoin-1.1.2.tgz";
        sha512 = "4EVb+w4rx+YfVM32HQX42AbbT7/1f5zwAYhIujKXKk8NQK+JfRVl3pqT3hjNn/L+RstigmGGKVwHA/P0wgITZw==";
      };
    };
    "vary-1.1.2" = {
      name = "vary";
      packageName = "vary";
      version = "1.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/vary/-/vary-1.1.2.tgz";
        sha1 = "2299f02c6ded30d4a5961b0b9f74524a18f634fc";
      };
    };
    "wif-2.0.6" = {
      name = "wif";
      packageName = "wif";
      version = "2.0.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/wif/-/wif-2.0.6.tgz";
        sha1 = "08d3f52056c66679299726fade0d432ae74b4704";
      };
    };
    "ws-8.3.0" = {
      name = "ws";
      packageName = "ws";
      version = "8.3.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/ws/-/ws-8.3.0.tgz";
        sha512 = "Gs5EZtpqZzLvmIM59w4igITU57lrtYVFneaa434VROv4thzJyV6UjIL3D42lslWlI+D4KzLYnxSwtfuiO79sNw==";
      };
    };
    "yallist-2.1.2" = {
      name = "yallist";
      packageName = "yallist";
      version = "2.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/yallist/-/yallist-2.1.2.tgz";
        sha1 = "1c11f9218f076089a47dd512f93c6699a6a81d52";
      };
    };
    "yallist-4.0.0" = {
      name = "yallist";
      packageName = "yallist";
      version = "4.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/yallist/-/yallist-4.0.0.tgz";
        sha512 = "3wdGidZyq5PB084XLES5TpOSRA3wjXAlIWMhum2kRcv/41Sn2emQ0dycQW4uZXLejwKvg6EsvbdlVL+FYEct7A==";
      };
    };
    "argparse-1.0.10" = {
      name = "argparse";
      packageName = "argparse";
      version = "1.0.10";
      src = fetchurl {
        url = "https://registry.npmjs.org/argparse/-/argparse-1.0.10.tgz";
        sha512 = "o5Roy6tNG4SL/FOkCAN6RzjiakZS25RLYFrcMttJqbdd8BWrnA+fGz57iN5Pb06pvBGvl5gQ0B48dJlslXvoTg==";
      };
    };
    "sprintf-js-1.0.3" = {
      name = "sprintf-js";
      packageName = "sprintf-js";
      version = "1.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/sprintf-js/-/sprintf-js-1.0.3.tgz";
        sha1 = "BOaSb2YolTVPPdAVIDYzuFcpfiw=";
      };
    };
  };
  args = {
    name = "mempool-backend";
    packageName = "mempool-backend";
    version = "2.4.0-dev";
    src = ./.;
    dependencies = [
      sources."sprintf-js-1.0.3"
      (sources."argparse-1.0.10" // {
        dependencies = [
          sources."sprintf-js-1.0.3"
        ];
      })
      sources."@mempool/bitcoin-3.0.3"
      sources."@mempool/electrum-client-1.1.8"
      sources."@types/node-14.14.20"
      sources."@types/ws-8.2.2"
      sources."accepts-1.3.7"
      sources."array-flatten-1.1.1"
      sources."axios-0.24.0"
      sources."base-x-3.0.9"
      sources."bech32-2.0.0"
      sources."bip174-2.0.1"
      sources."bitcoinjs-lib-6.0.1"
      sources."body-parser-1.19.0"
      sources."bs58-4.0.1"
      sources."bs58check-2.1.2"
      sources."bytes-3.1.0"
      sources."cipher-base-1.0.4"
      (sources."content-disposition-0.5.3" // {
        dependencies = [
          sources."safe-buffer-5.1.2"
        ];
      })
      sources."content-type-1.0.4"
      sources."cookie-0.4.0"
      sources."cookie-signature-1.0.6"
      sources."create-hash-1.2.0"
      sources."crypto-js-4.0.0"
      sources."debug-2.6.9"
      sources."denque-2.0.1"
      sources."depd-1.1.2"
      sources."destroy-1.0.4"
      sources."ee-first-1.1.1"
      sources."encodeurl-1.0.2"
      sources."escape-html-1.0.3"
      sources."etag-1.8.1"
      (sources."express-4.17.1" // {
        dependencies = [
          sources."safe-buffer-5.1.2"
        ];
      })
      sources."finalhandler-1.1.2"
      sources."follow-redirects-1.14.7"
      sources."forwarded-0.1.2"
      sources."fresh-0.5.2"
      sources."generate-function-2.3.1"
      sources."hash-base-3.1.0"
      (sources."http-errors-1.7.2" // {
        dependencies = [
          sources."inherits-2.0.3"
        ];
      })
      sources."iconv-lite-0.4.24"
      sources."inherits-2.0.4"
      sources."ipaddr.js-1.9.1"
      sources."is-property-1.0.2"
      sources."locutus-2.0.15"
      sources."long-4.0.0"
      sources."lru-cache-6.0.0"
      sources."md5.js-1.3.5"
      sources."media-typer-0.3.0"
      sources."merge-descriptors-1.0.1"
      sources."methods-1.1.2"
      sources."mime-1.6.0"
      sources."mime-db-1.45.0"
      sources."mime-types-2.1.28"
      sources."ms-2.0.0"
      (sources."mysql2-2.3.3" // {
        dependencies = [
          sources."iconv-lite-0.6.3"
        ];
      })
      (sources."named-placeholders-1.1.2" // {
        dependencies = [
          sources."lru-cache-4.1.5"
          sources."yallist-2.1.2"
        ];
      })
      sources."negotiator-0.6.2"
      sources."node-worker-threads-pool-1.4.3"
      sources."on-finished-2.3.0"
      sources."parseurl-1.3.3"
      sources."path-to-regexp-0.1.7"
      sources."proxy-addr-2.0.6"
      sources."pseudomap-1.0.2"
      sources."qs-6.7.0"
      sources."range-parser-1.2.1"
      sources."raw-body-2.4.0"
      sources."readable-stream-3.6.0"
      sources."ripemd160-2.0.2"
      sources."safe-buffer-5.2.1"
      sources."safer-buffer-2.1.2"
      (sources."send-0.17.1" // {
        dependencies = [
          sources."ms-2.1.1"
        ];
      })
      sources."seq-queue-0.0.5"
      sources."serve-static-1.14.1"
      sources."setprototypeof-1.1.1"
      sources."sha.js-2.4.11"
      sources."sqlstring-2.3.2"
      sources."statuses-1.5.0"
      sources."string_decoder-1.3.0"
      sources."toidentifier-1.0.0"
      sources."type-is-1.6.18"
      sources."typeforce-1.18.0"
      sources."typescript-4.4.4"
      sources."unpipe-1.0.0"
      sources."util-deprecate-1.0.2"
      sources."utils-merge-1.0.1"
      sources."varuint-bitcoin-1.1.2"
      sources."vary-1.1.2"
      sources."wif-2.0.6"
      sources."ws-8.3.0"
      sources."yallist-4.0.0"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "Bitcoin mempool visualizer and blockchain explorer backend";
      homepage = "https://mempool.space";
      license = "GNU Affero General Public License v3.0";
    };
    production = true;
    bypassCache = true;
    reconstructLock = false;
  };
in
{
  args = args;
  sources = sources;
  tarball = nodeEnv.buildNodeSourceDist args;
  package = nodeEnv.buildNodePackage args;
  shell = nodeEnv.buildNodeShell args;
  nodeDependencies = nodeEnv.buildNodeDependencies (lib.overrideExisting args {
    src = stdenv.mkDerivation {
      name = args.name + "-package-json";
      src = nix-gitignore.gitignoreSourcePure [
        "*"
        "!package.json"
        "!package-lock.json"
      ] args.src;
      dontBuild = false;
      buildPhase = "exit 1";
      installPhase = "mkdir -p $out; cp -r ./* $out;";
    };
  });
}