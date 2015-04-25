/*
 * Enable WebSockets connection
 *
 */

var websocket;
var nick;

var login, messagelist;

$(document).ready(function () {
  loadLoginUI();
});

var connect = function () {

  nick      = $("#nick").val();
  websocket = websocket || new WebSocket("ws://nils.cc:8099/" + encodeURIComponent(nick));

  websocket.onopen = function (e) {
    login = $("#login");
    $("#login").replaceWith( loadChatUI() );
  };

  websocket.onerror = function (e) {
    //console.error("WebSocket error:", e);
  };

  websocket.onclose = function (c) {
    $("#chat").replaceWith( loadLoginUI() );
    websocket = undefined;
  };

  websocket.onmessage = function (e) {
    showIncomingMessage(JSON.parse(e.data));
  };
}

/*
 * Load chat UI
 *
 */

var loadLoginUI = function () {
  login = login || $("#login");

  var submit = login.find(".submit");
  submit.click(connect);

  login.find("input").keypress(function (e) {
    switch (e.keyCode) {
      case 13: { submit.click(); break; }
    }
  });

  login.focus();

  return login;
}

var loadChatUI = function () {

  var chat = $( "<div id=\"chat\">"
              +   "<ul class=\"message-list\"></ul>"
              +   "<div><input type=\"text\"> <button>Send</button></div>"
              + "</div>"
              );

  var input = chat.find("input"),
      button = chat.find("button");

  messagelist = chat.find(".message-list");

  button.click(function () {
    websocket.send( input.val() );
    input.val("").focus();
  });

  input.keypress(function (e) {
    switch (e.keyCode) {
      case 13: { button.click(); break; }
    }
  });

  input.focus();

  return chat;
}

var showIncomingMessage = function (msg) {

  var li = $( "<li class=\"chat-message\">"
            +   "<span class=\"from\"></span>"
            +   "<span class=\"message\"></span>"
            + "</li>"
            );

  li.find(".from").text(msg.f);
  li.find(".message").text(msg.m);

  // get current scroll location
  var atBottom = (messagelist.scrollTop() + 1)
              >= (messagelist.prop("scrollHeight") - messagelist.innerHeight());

  messagelist.append(li);

  // update scroll position
  if (atBottom) {
    messagelist.scrollTop(function () {
      return this.scrollHeight - $(this).innerHeight();
    });
  }
}
