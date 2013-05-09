Pixel Party
================

Like the original Pixel Party (http://pixelparty.me/) but with websockets instead of AJAX.

HTML and Javascript borrowed from http://pixelparty.me/. Websocket code borrowed from https://github.com/extend/cowboy.

To compile this you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Then point your browser to the indicated URL to open a websocket client.
Not all browsers support websockets. It was tested with Chromium.
