import { Container, Grid } from '@mui/material'
import useWebSocket, { ReadyState } from 'react-use-websocket'
import './App.css'
import mapImage from './assets/map.png'
import BotResponse from './models/BotResponse'
import ChatMessage from './models/ChatMessage'
import React, {useEffect, useRef} from 'react'
import { MessageAuthor } from './models/MessageAuthor'
import Chat from "./components/Chat";

function getAsciiValues(text: String) {
  const asciiCodes = []

  for (let i = 0; i < text.length; i++) {
    asciiCodes.push(text.charCodeAt(i))
  }

  return asciiCodes
}

function App() {
  const chatBotRef = useRef();
  const gameBotRef = useRef();

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
  const defaultChatMessages : ChatMessage[] = [
    {
      message:
        'Bonjour, je suis le bot du Tour, BDT, pour les intimes, conseiller sur le Tour de France. En quoi puis-je vous etre utile ?',
      author: MessageAuthor.BOT,
      timestamp: new Date(),
    },
  ]

  const onSendChatBotMessage = (message: ChatMessage) => {
    sendMessage(JSON.stringify({ message: getAsciiValues(message.message.toLowerCase()) }))
  }

  const onSendGameBotMessage = (message: ChatMessage) => {
    alert("ToDo : send game bot message (" + message.message + ")")
  }

  // Bot response
  useEffect(() => {
    if (lastMessage !== null) {
      const stringResponse = parsedMessage(lastMessage.data)

      const chatMessage: ChatMessage = {
        message: stringResponse,
        author: MessageAuthor.BOT,
        timestamp: new Date(),
      }

      chatBotRef.current.onReceiveMessage(chatMessage);
    }
  }, [lastMessage])

  return (
    <Container maxWidth={false}>
      <Grid item xs={12} textAlign={'center'}>
        <h1>Tour de France</h1>
        <small>
          {readyState ? (
            <p>Connexion avec le bot : {connectionStatus}</p>
          ) : (
            <p>Serveur du bot introuvable</p>
          )}
        </small>
      </Grid>

      <Grid container justifyContent="center">
        <Grid container xs={5} justifyContent="center" alignItems="center">
          <img src={mapImage} id="map-image" alt="Plateau de jeu tour de france" />
        </Grid>

        <Grid container xs={5}>
          <Chat ref={chatBotRef} height="40vh" onSendMessage={onSendChatBotMessage}
                title="Discussion avec le bot du tour" submitDisabled={readyState !== ReadyState.OPEN}
                defaultMessages={defaultChatMessages}/>
        </Grid>
      </Grid>

      <Grid container justifyContent="center" sx={{mt:5}}>
        <Grid container xs={5} justifyContent="center" alignItems="center">
          <p>ToDo : Add teams table</p>
        </Grid>

        <Grid container xs={5}>
          <Chat ref={gameBotRef} height="24vh" onSendMessage={onSendGameBotMessage} title="Bot de jeu" placeholder="Entrer une instruction de jeu"/>
        </Grid>
      </Grid>
    </Container>
  )
}

export default App
