ChatServer
==========
This is a server that implements the backend for a chat server. Clients can connect and join/leave multiple chatrooms, sending messages to all clients subscribed to that chatroom.

##Compile and Run:
1. git clone https://github.com/ghodgins/ChatServer.git
2. cd src/
3. ghc Main.hs
4. ./Main

##Telnet clients to test:
As standard the program runs on port 44444 so the following command can be used:
- ```telnet localhost 44444```

The following commands can then be used in the telnet session (do each number for each client then move onto the next step).

###Client 1
1. JOIN_CHATROOM:Distro\nCLIENT_IP:0\nPORT:0\nCLIENT_NAME:Conor\n\n
2. CHAT:0\nJOIN_ID:0\nCLIENT_NAME:Conor\nMESSAGE:Hello my name is Conor blah blah blah\n\n
3. LEAVE_CHATROOM:0\nJOIN_ID:0\nCLIENT_NAME:Conor\n\n
4. DISCONNECT:0\nPORT:0\nCLIENT_NAME:Conor\n\n

###Client 2
1. JOIN_CHATROOM:Distro\nCLIENT_IP:0\nPORT:0\nCLIENT_NAME:James\n\n
2. CHAT:0\nJOIN_ID:1\nCLIENT_NAME:James\nMESSAGE:Hello my name is James blah blah blah\n\n
3. LEAVE_CHATROOM:0\nJOIN_ID:1\nCLIENT_NAME:James\n\n
4. DISCONNECT:0\nPORT:0\nCLIENT_NAME:James\n\n

