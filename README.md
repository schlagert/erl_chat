Erlang Backend for Elm-based chat code kata
===========================================

JSON Data Types (as Elm types)
------------------------------

```elm
type alias Participant =
    { id : String
    , name : String
    }

type alias ChatRoom =
    { id : String
    , title : String
    }

type alias ChatRegistration =
    { participant : Participant
    , chatRoom : ChatRoom
    }

type alias Message =
    { message : String
    }

type alias MessageLog =
    { messageLog : String
    }

type alias Id =
    String
```

REST Services
-------------

* `GET "/chatRoom"`

  Retrieve a list of chat rooms as `List ChatRoom`.

* `GET "/chatRoom/:chatRoomId"`

  Retrieve the chat history for a specific room as `MessageLog`.

* `POST "/participant"`

  Create a new participant with the specified name on the server. The id of
  the new participant is returned in the response. Both direction use the
  `Participant` type.

* `POST "/chatRoom"`

  Create a new chat room on the server with the specified title. The id of the
  new room is returned in the response. Both direction use the `ChatRoom` type.

WebSocket Service
-----------------

WebSocket `"/chat“` e.g. "ws://localhost:4567/chat"

The websocket understands the following messages:

* `registration:ChatRegistration`

  Connect the session with a participant and a chat room. May be repeated at any
  time.

* `message:Message`

  Send a message to the session's chat room.

Examples
--------

```
registration:{"participant":{"id":5, "name":"Klaus"},
              "chatRoom":{"id":3, "title":"Room 3"}}

message:{"message":"Hallo an alle zusammen"}
```
