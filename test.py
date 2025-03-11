import socketio

import logging
import requests
import json
import re
from socketIO_client import SocketIO, LoggingNamespace

sio = socketio.Client(logger=True, engineio_logger=True)

logging.basicConfig(
    format="%(asctime)s %(message)s",
    level=logging.DEBUG,
)
curl_string = "curl 'wss://www.overleaf.com/socket.io/1/websocket/MrEH3JtFrdrhYiYxFKuh?projectId=67cb6eea861396a27bbc7aab&esh=1&ssp=1' -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:135.0) Gecko/20100101 Firefox/135.0' -H 'Accept: */*' -H 'Accept-Language: en-US,en;q=0.5' -H 'Accept-Encoding: gzip, deflate, br, zstd' -H 'Sec-WebSocket-Version: 13' -H 'Origin: https://www.overleaf.com' -H 'Sec-WebSocket-Extensions: permessage-deflate' -H 'Sec-WebSocket-Key: fW1hi92lc9yWy7AcoAI0iA==' -H 'Connection: keep-alive, Upgrade' -H 'Cookie: overleaf_session2=s%3A11Va3hIwoXyaWUF_7geULEBGH5dUHPMr.rgQT%2FXONrClS0o0OyW1p4a4Kormb2v6UyC86jtY%2FMR0; __stripe_mid=ed98fee4-3a6c-4417-bac5-2b0855311097295942; GCLB=CMD37ejsnuTxkAEQAw; __stripe_sid=51ed3af8-6b52-4e05-a7c3-ece7c593bcd5c4a119' -H 'Sec-Fetch-Dest: empty' -H 'Sec-Fetch-Mode: websocket' -H 'Sec-Fetch-Site: same-origin' -H 'Pragma: no-cache' -H 'Cache-Control: no-cache' -H 'Upgrade: websocket'"


def parse_curl(curl_string):
    url_regex = re.compile(r"curl '(.*?)'")
    header_regex = re.compile("(?:-H '(.*?): (.*?)')+")

    url = url_regex.findall(curl_string)[0]
    headers = dict(header_regex.findall(curl_string))
    # del headers["Sec-WebSocket-Key"]

    return url, dict(Cookie=headers["Cookie"])


id = "67cb6eea861396a27bbc7aab"


@sio.event()
def connect():
    logging.info("Connected")


@sio.event()
def joinProjectResponse(data):
    print(data)


def hello():
    url, headers = parse_curl(curl_string)
    test = requests.get(
        f"https://www.overleaf.com/socket.io/1/?projectId={id}&esh=1&ssp=1",
        headers=headers,
    )
    print(test.text)
    socket_id = test.text.split(":")[0]

    url = f"https://www.overleaf.com/socket.io/1/websocket/{socket_id}?projectId={id}&esh=1&ssp=1"

    url = f"https://www.overleaf.com?projectId={id}&esh=1&ssp=1"
    try:
        sio.connect(
            url,
            transports=["websocket"],
            headers=headers,
            namespace=f"websocket/{socket_id}",
            wait=True,
        )
        sio.wait()
    except socketio.exceptions.ConnectionError as e:
        print(e)


if __name__ == "__main__":
    hello()
    # with connect(
    #     url,
    #     origin="https://www.overleaf.com",
    #     user_agent_header="User-Agent:Mozilla/5.0 (X11; Linux x86_64; rv:135.0) Gecko/20100101 Firefox/135.0",
    #     additional_headers=headers,
    # ) as websocket:
    #     # websocket.send(
    #     #     f'5:2+::{{"name":"joinDoc","args":["{id}",{{"encodeRanges":true}}]}}'
    #     # )
    #     while True:
    #         message = websocket.recv()
    #         parts = message.split(":::")
    #         if len(parts) == 2:
    #             proto, message = message.split(":::")
    #             print("message", message)
    #             try:
    #                 message = json.loads(message)
    #             except:
    #                 continue
    #             match message["name"]:
    #                 case "serverPing":
    #                     print(
    #                         json.dumps(
    #                             dict(
    #                                 name="clientPong",
    #                                 args=message["args"] + message["args"][-2:],
    #                             )
    #                         )
    #                     )

    #                     print(f">>> Got ping", message)
    #                     websocket.send(
    #                         f"{proto}:::"
    #                         + json.dumps(
    #                             dict(
    #                                 name="clientPong",
    #                                 args=message["args"] + message["args"][-2:],
    #                             )
    #                         )
    #                     )
    #                 case _:
    #                     print(f">>> Unknown: {message}")
