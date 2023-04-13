import {Button, Grid, TextField} from '@mui/material'
import useWebSocket, {ReadyState} from 'react-use-websocket'
import './App.css'
import mapImage from './assets/map.png'
import BotResponse from './models/BotResponse'
import ChatMessage from './models/ChatMessage'
import ChatMessageComponent from "./components/ChatMessageComponent";
import React, {useEffect, useState} from "react";
import {MessageAuthor} from "./models/MessageAuthor";

function App() {
  const { sendMessage, lastMessage, readyState } = useWebSocket(
    import.meta.env.VITE_API_HOST + '/bot',
    {
      retryOnError: true,
      reconnectInterval: 1000,
    }
  )

  const connectionStatus = {
    [ReadyState.CONNECTING]: 'en cours de connexion ...',
    [ReadyState.OPEN]: 'connecté',
    [ReadyState.CLOSING]: 'en cours de déconnexion ...',
    [ReadyState.CLOSED]: 'déconnecté',
    [ReadyState.UNINSTANTIATED]: 'Uninstantiated',
  }[readyState]

  const parsedMessage = (res: string) => {
    const data: BotResponse = JSON.parse(res)
    return data.message.map(m => m.join(' ')).join('\n')
  }

  // Discussion messages
  const [messages, setMessages] = useState<ChatMessage[]>([
    {message : "Bonjour, je suis le bot du Tour, BDT, pour les intimes, conseiller sur le Tour de France. En quoi puis-je vous etre utile ?", author: MessageAuthor.BOT, timestamp : new Date()}
  ]);

  // User message
  let [userMessage, setUserMessage] = useState<String>("");
  const sendMessageClick = () => {
    if(userMessage == undefined){
      return;
    }

    sendMessage(JSON.stringify({ message: userMessage.split(' ') }));

    const chatMessage : ChatMessage = {message: userMessage, author : MessageAuthor.USER, timestamp : new Date()};
    setMessages([...messages, chatMessage]);

    setUserMessage("");
  }

  // Bot response
  useEffect(() => {
    if (lastMessage !== null) {
      const stringResponse = parsedMessage(lastMessage.data);

      const chatMessage : ChatMessage = {message: stringResponse, author : MessageAuthor.BOT, timestamp : new Date()};
      setMessages([...messages, chatMessage]);
    }
  }, [lastMessage]);

  return (
    <Grid container spacing={5} sx={{px:0, py:0}}>
      <Grid item xs={12} textAlign={"center"}>
        <h1>Tour de France</h1>
        <small>{readyState ? <p>Connexion avec le bot : {connectionStatus}</p> : <p>Serveur du bot introuvable</p>}</small>
      </Grid>

      <Grid container justifyContent="center">
        <Grid item xs={5}>
          <img src={mapImage} id="map-image" alt="Plateau de jeu tour de france" />
        </Grid>

        <Grid item xs={5} id="chat-bot-card" sx={{px:0}}>
          <Grid item xs={12}>
            <h2 className="text-center">Discussion avec le bot du tour ...</h2>
          </Grid>

          <Grid container direction="column" spacing={0} height="42vh" >
            <div style={{overflowY: "scroll", height: "100%"}}>
              {
                messages.map((chatMessage, iChatMessage) => {
                  return <ChatMessageComponent {...chatMessage} key={iChatMessage}/>
                })
              }
            </div>
          </Grid>

          <Grid container spacing={2} alignItems="center" sx={{px:3, py:1}}>
            <Grid item xs={10}>
              <TextField label="Entrer votre message ..." variant="filled" fullWidth
                         value={userMessage} onChange={(e) => setUserMessage(e.target.value ?? "")}/>
            </Grid>
            <Grid item xs={2}>
              <Button variant="contained" onClick={sendMessageClick} disabled={readyState !== ReadyState.OPEN || userMessage == ""} fullWidth>Envoyer</Button>
            </Grid>
          </Grid>
        </Grid>
      </Grid>
    </Grid>




/*    <div className="App">
      <h1>Vite + React</h1>
      <div className="card">
        {readyState && <p>readyState: {connectionStatus}</p>}
        <button onClick={handleClick} disabled={readyState !== ReadyState.OPEN}>
          Send Message
        </button>
        <div>{lastMessage && <p>lastMessage: {parsedMessage(lastMessage.data)}</p>}</div>
      </div>
    </div>*/
  )
}

export default App
