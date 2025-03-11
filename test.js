const io = require("socket.io-client");
async function get_url() {
    const url = await fetch(
        "https://www.overleaf.com/socket.io/1/?projectId=67cb6eea861396a27bbc7aab&esh=1&ssp=1",
        {
            credentials: "include",
            headers: {
                "User-Agent":
                    "Mozilla/5.0 (X11; Linux x86_64; rv:135.0) Gecko/20100101 Firefox/135.0",
                Accept: "*/*",
                "Accept-Language": "en-US,en;q=0.5",
                "Sec-Fetch-Dest": "empty",
                "Sec-Fetch-Mode": "cors",
                "Sec-Fetch-Site": "same-origin",
                Cookie: "overleaf_session2=s%3A11Va3hIwoXyaWUF_7geULEBGH5dUHPMr.rgQT%2FXONrClS0o0OyW1p4a4Kormb2v6UyC86jtY%2FMR0; __stripe_mid=ed98fee4-3a6c-4417-bac5-2b0855311097295942; GCLB=CMD37ejsnuTxkAEQAw; __stripe_sid=51ed3af8-6b52-4e05-a7c3-ece7c593bcd5c4a119",
            },
            referrer:
                "https://www.overleaf.com/project/67cb6eea861396a27bbc7aab",
            method: "GET",
            mode: "cors",
        },
    );
    return (await url.text()).split(":")[0];
}
const socket = io.connect("https://www.overleaf.com", {
    query: "projectId=67cb6eea861396a27bbc7aab&esh=1&ssp=1",
    "auto connect": false,
    extraHeaders: {
        Cookie: "overleaf_session2=s%3A11Va3hIwoXyaWUF_7geULEBGH5dUHPMr.rgQT%2FXONrClS0o0OyW1p4a4Kormb2v6UyC86jtY%2FMR0; __stripe_mid=ed98fee4-3a6c-4417-bac5-2b0855311097295942; GCLB=CMD37ejsnuTxkAEQAw; __stripe_sid=51ed3af8-6b52-4e05-a7c3-ece7c593bcd5c4a119",
    },
});

socket.on("connect", () => {
    console.log("con");
});
socket.on("disconnect", (reason) => {
    console.log("dis");
});
socket.on("error", (err) => {
    console.log("err", err);
});
socket.on("connect_failed", (err) => {
    console.log("fail");
});
socket.on("joinProjectResponse", (body) => {
    console.log("join");
});
socket.on("connectionRejected", (err) => {
    console.log("rej");
});
socket.on("reconnectGracefully", () => {
    console.log("rec");
});
socket.on("forceDisconnect", (_, delay) => {
    console.log("forc");
});
socket.on(
    "serverPing",
    (counter, timestamp, serverTransport, serverSessionId) => {
        console.log("ping");
    },
);
socket.socket.connect();
console.log(socket);
