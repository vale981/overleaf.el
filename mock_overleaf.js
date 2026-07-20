const http = require('http');
const express = require('express');
const socketio = require('socket.io');

const app = express();
const server = http.createServer(app);
const io = socketio.listen(server);

// Configure socket.io to match Overleaf's old settings
io.configure(function () {
  io.set('transports', ['websocket', 'xhr-polling']);
  io.set('authorization', function (handshakeData, callback) {
    // Skip actual session check for mock
    callback(null, true);
  });
});

const mockProject = {
  _id: 'project1',
  name: 'Mock Project',
  rootFolder: [
    {
      _id: 'folder1',
      name: 'rootFolder',
      docs: [
        { _id: 'doc1', name: 'main.tex' }
      ],
      folders: []
    }
  ]
};

const mockDocs = {
  'doc1': {
    lines: ['\\documentclass{article}', '\\begin{document}', 'Hello Overleaf!', '\\end{document}'],
    version: 0
  }
};

io.sockets.on('connection', function (socket) {
  console.log('Client connected:', socket.id);
  
  const projectId = socket.handshake.query.projectId;
  
  // 1. Send joinProjectResponse
  socket.emit('joinProjectResponse', {
    publicId: 'P.' + socket.id,
    project: mockProject,
    permissionsLevel: 'owner',
    protocolVersion: 2
  });

  socket.on('joinDoc', function (docId, fromVersion, options, callback) {
    console.log('joinDoc:', docId);
    const doc = mockDocs[docId];
    if (doc) {
      // Overleaf 0.9 style joinDoc response is usually sent via message ID 6
      // But let's follow the callback pattern if provided, or emit 'initialLoad'
      // The elisp code expects '6:::null,[lines],version'
      socket.send('6:::null,' + JSON.stringify(doc.lines) + ',' + (doc.version + 1));
      if (typeof callback === 'function') {
          callback(null, doc.lines, doc.version, [], {});
      }
    } else {
      if (typeof callback === 'function') callback('Doc not found');
    }
  });

  socket.on('applyOtUpdate', function (docId, update, callback) {
    console.log('applyOtUpdate:', docId, update);
    const doc = mockDocs[docId];
    if (doc) {
      // Simulate a concurrent edit from another user that happened "just before" or "during"
      // we received this update. We'll send it BEFORE we acknowledge the current update
      // if the user sends a specific flag or just for testing purposes.
      
      if (update.op && update.op[0] && update.op[0].i === 'CONCURRENT_TEST') {
          console.log('Triggering concurrent edit simulation');
          const concurrentOp = [{p: 0, i: 'SERVER_EDIT ' + (doc.version + 1) + '\n'}];
          doc.version++;
          socket.emit('otUpdateApplied', {
              v: doc.version,
              doc: docId,
              op: concurrentOp,
              u: 'other-user'
          });
      }

      doc.version = doc.version + 1;
      // ACK the client's update
      if (typeof callback === 'function') callback();
      
      // Broadcast the client's update to others
      socket.broadcast.to(docId).emit('otUpdateApplied', {
          v: doc.version,
          doc: docId,
          op: update.op,
          u: 'mock-user'
      });
    }
  });

  socket.on('clientTracking.updatePosition', function (cursorData, callback) {
    // console.log('Position update:', cursorData);
    if (typeof callback === 'function') callback();
  });

  socket.on('disconnect', function () {
    console.log('Client disconnected');
  });
});

const PORT = 3000;
server.listen(PORT, '127.0.0.1', () => {
  console.log(`Mock Overleaf server listening on http://127.0.0.1:${PORT}`);
});
