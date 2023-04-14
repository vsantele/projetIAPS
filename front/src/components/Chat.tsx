import React, {forwardRef, useImperativeHandle, useState} from "react";
import {Button, Grid, TextField} from "@mui/material";
import ChatMessageComponent from "./ChatMessageComponent";
import ChatMessage from "../models/ChatMessage";
import {MessageAuthor} from "../models/MessageAuthor";

interface ChatProps {
  onSendMessage(message: ChatMessage): void,
  submitDisabled: boolean | null,
  title: string,
  placeholder: string | null,
  height: string,
  defaultMessages: ChatMessage[] | null
}

const Chat = forwardRef((props: ChatProps, ref) => {
  // Discussion messages
  const [messages, setMessages] = useState<ChatMessage[]>(props.defaultMessages ?? []);

  // User message
  let [userMessage, setUserMessage] = useState<String>("");

  const sendMessageClick = () => {
    if (userMessage == undefined) {
      return;
    }

    const chatMessage: ChatMessage = {message: userMessage, author: MessageAuthor.USER, timestamp: new Date()};
    setMessages([...messages, chatMessage]);
    props?.onSendMessage(chatMessage);

    setUserMessage("");
  }

  useImperativeHandle(ref, () => ({
    onReceiveMessage,
  }));

  const onReceiveMessage = (message: ChatMessage) => {
    setMessages([...messages, message]);
  }

  return (
    <Grid container>
      <Grid item xs={12} sx={{m:0, p:0}}>
        <h2 className="text-center">{props.title}</h2>
      </Grid>

      <Grid container direction="column" spacing={0} height={props.height ?? "100%"}>
        <Grid item style={{overflowY: "scroll", height: "100%"}}>
          {messages.map(chatMessage => {
            return (
              <ChatMessageComponent {...chatMessage} key={chatMessage.timestamp.toString()}/>
            )
          })}
        </Grid>
      </Grid>

      <Grid container spacing={2} alignItems="center" sx={{px: 3, my: 0}}>
        <Grid item xs={10} sx={{m: 0, p:0}}>
          <TextField size="small" label={props.placeholder ?? "Entrer votre message ..."} variant="filled" fullWidth
                     value={userMessage} onChange={(e) => setUserMessage(e.target.value ?? "")}/>
        </Grid>
        <Grid item xs={2} >
          <Button variant="contained" onClick={sendMessageClick} disabled={(props.submitDisabled != null && props.submitDisabled) || userMessage == ""} fullWidth>Envoyer</Button>
        </Grid>
      </Grid>
    </Grid>
  )
});

export default Chat;