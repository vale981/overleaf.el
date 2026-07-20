const http = require('http');
const express = require('express');
const socketio = require('socket.io');

const app = express();
const server = http.createServer(app);
const io = socketio.listen(server);

io.configure(function () {
  io.set('transports', ['websocket']);
  io.set('log level', 1);
});

let docContent = "Initial content\n".repeat(100);
let docVersion = 0;

function applyOp(content, op) {
    let newContent = content;
    // Overleaf ops are usually a list of {p: pos, i: str} or {p: pos, d: str}
    // For simplicity, we assume they are sorted or independent in the mock
    // Real Overleaf OT is more complex, but we'll simulate the impact
    op.forEach(subOp => {
        const p = subOp.p;
        if (subOp.i !== undefined) {
            newContent = newContent.slice(0, p) + subOp.i + newContent.slice(p);
        } else if (subOp.d !== undefined) {
            newContent = newContent.slice(0, p) + newContent.slice(p + subOp.d.length);
        }
    });
    return newContent;
}

io.sockets.on('connection', function (socket) {
  console.log('Stress client connected:', socket.id);
  
  socket.emit('joinProjectResponse', {
    project: { _id: 'stress-project', rootFolder: [{ docs: [{ _id: 'stress-doc', name: 'stress.tex' }] }] },
    permissionsLevel: 'owner'
  });

  socket.on('joinDoc', function (docId, fromVersion, options, callback) {
    if (typeof callback === 'function') {
        callback(null, docContent.split('\n'), docVersion, [], {});
    }
  });

  socket.on('applyOtUpdate', function (docId, update, callback) {
    // Random delay to simulate network latency
    const delay = Math.random() * 50;
    setTimeout(() => {
        docContent = applyOp(docContent, update.op);
        docVersion++;
        
        if (typeof callback === 'function') callback();
        
        // Broadcast to others
        socket.broadcast.emit('otUpdateApplied', {
            v: docVersion,
            doc: docId,
            op: update.op,
            u: 'stress-user'
        });
    }, delay);
  });
});

// Chaos Monkey: randomly edit the document and push to all clients
setInterval(() => {
    if (io.sockets.sockets.length === 0) return;
    
    const pos = Math.floor(Math.random() * docContent.length);
    const chars = "abcdefghijklmnopqrstuvwxyz \n";
    const text = chars[Math.floor(Math.random() * chars.length)];
    const op = [{ p: pos, i: text }];
    
    docContent = applyOp(docContent, op);
    docVersion++;
    
    console.log(`Chaos Edit: v${docVersion} at ${pos}: ${JSON.stringify(text)}`);
    io.sockets.emit('otUpdateApplied', {
        v: docVersion,
        doc: 'stress-doc',
        op: op,
        u: 'chaos-monkey'
    });
}, 200); // 5 edits per second

const PORT = 3001;
server.listen(PORT, '127.0.0.1', () => {
  console.log(`Chaos Overleaf server listening on http://127.0.0.1:${PORT}`);
});
