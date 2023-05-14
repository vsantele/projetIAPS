import { Box, Container, Grid } from '@mui/material'
import useWebSocket, { ReadyState } from 'react-use-websocket'
import './App.css'
import mapImage from './assets/map.png'
import BotResponse from './models/BotResponse'
import ChatMessage from './models/ChatMessage'
import { MouseEvent, useEffect, useState } from 'react'
import { MessageAuthor } from './models/MessageAuthor'
import Chat from './components/Chat'
import MapPosition from './models/MapPosition'
import Board from './temp_board.json'

enum Team {
  ITALY,
  NETHERLANDS,
  BELGIUM,
  GERMANY,
}

const teams = [
  { id: Team.ITALY, name: 'Italie', cards: [1, 2, 4, 8, 12] },
  { id: Team.NETHERLANDS, name: 'Pays-Bas', cards: [5, 7, 10, 11, 12] },
  { id: Team.BELGIUM, name: 'Belgique', cards: [3, 5, 7, 9, 12] },
  { id: Team.GERMANY, name: 'Allemange', cards: [1, 6, 7, 8, 10] },
]

function getAsciiValues(text: string) {
  const asciiCodes = []

  for (let i = 0; i < text.length; i++) {
    asciiCodes.push(text.charCodeAt(i))
  }

  return asciiCodes
}

function AppCoordsGen() {
  const { sendMessage, lastMessage, readyState } = useWebSocket(
    (import.meta.env.VITE_WEBSOCKET_HOST ?? '') + '/bot',
    {
      retryOnError: false,
      reconnectInterval: 1000,
    }
  )

  // Discussion messages
  const defaultChatMessages: ChatMessage[] = [
    {
      message:
        'Bonjour, je suis le bot du Tour, BDT, pour les intimes, conseiller sur le Tour de France. En quoi puis-je vous etre utile ?',
      author: MessageAuthor.BOT,
      timestamp: new Date(),
    },
  ]
  const defaultInstructions: ChatMessage[] = [
    {
      message: "A l'équipe Belgique 2-avance-5",
      author: MessageAuthor.BOT,
      timestamp: new Date(),
    },
  ]
  const [botMessages, setBotMessages] = useState<ChatMessage[]>(defaultChatMessages)
  const [instructions, setInstruction] = useState<ChatMessage[]>(defaultInstructions)

  const [positions, setPositions] = useState<MapPosition[]>(Board ?? [])
  const [clickCount, setClickCount] = useState<number>(
    Board == null ? 0 : Object.keys(Board).length + 1
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

  const handleSendChatBotMessage = (message: string) => {
    setBotMessages(messages => [
      ...messages,
      { message, author: MessageAuthor.USER, timestamp: new Date() },
    ])
    sendMessage(JSON.stringify({ message: getAsciiValues(message.toLowerCase()) }))
  }

  const onSendGameBotMessage = (message: string) => {
    alert('ToDo : send game bot message (' + message + ')')
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

      setBotMessages(messages => [...messages, chatMessage])
    }
  }, [lastMessage])

  const board: (['d', number, number] | ['s', number])[] = [
    ['d', 8, 3],
    ['s', 2],
    ['d', 8, 2],
    ['d', 4, 3],
    ['d', 3, 4],
    ['s', 2],
    ['d', 8, 4],
    ['d', 27, 2],
    ['s', 2],
    ['d', 8, 2],
    ['d', 3, 1],
    ['d', 8, 2],
    ['d', 5, 3],
    ['s', 2],
    ['d', 4, 3],
    ['d', 1, 3],
  ]

  function getCaseFromIClick(iClick: number) {
    let zValue = 1
    let ceQuonADejaAvance = 1

    for (let i = 0; i < board.length; i++) {
      const boardRule = board[i]

      if (boardRule[0] === 's') {
        zValue += boardRule[1] * 10
        ceQuonADejaAvance += boardRule[1]
      } else {
        for (let z = 0; z < boardRule[1]; z++) {
          for (let x = 0; x <= boardRule[2]; x++) {
            if (iClick == ceQuonADejaAvance) {
              return [zValue * 10, x]
            }

            ceQuonADejaAvance++
          }

          zValue++
        }
      }
    }
  }

  const onClickImage = (event: MouseEvent<HTMLDivElement>) => {
    const elem = document.getElementById('map-area') //outer starts at your elem then walks out
    const bounding = elem!.getBoundingClientRect()
    const playerImageSize = 10

    const xRatio = ((event.clientX - playerImageSize / 2 - bounding.left) / bounding.width) * 100
    const yRatio = ((event.clientY - playerImageSize / 2 - bounding.top) / bounding.height) * 100

    console.log(xRatio + '%, ' + yRatio + '%')

    const caseFromClick = getCaseFromIClick(clickCount) as [number, number]
    const newPosition: MapPosition = {
      mapXRatio: xRatio,
      mapYRatio: yRatio,
      playerForward: caseFromClick[0],
      playerLateral: caseFromClick[1],
    }

    setPositions([...positions, newPosition])

    setClickCount(clickCount + 1)
  }


  console.log(positions);

  return (
    <Container maxWidth="xl">
      <Grid container justifyContent="center" alignContent="center" spacing={1}>
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
        <Grid item xs={12} md={6} xl={8} textAlign="center">
          <div id="map-area" onClick={onClickImage}>
            <img src={mapImage} id="map-image" alt="Plateau de jeu tour de france" />

            {
              positions.map((position, iPos) => (
              <div
                key={iPos}
                className="player"
                style={{ left: position.mapXRatio + '%', top: position.mapYRatio + '%', background:"yellow", fontSize:"10px" }}
              >
                {position.playerForward + ", " + position.playerLateral}
              </div>
            ))}
          </div>
        </Grid>
        <Grid item xs={12} md={6} xl={4}>
          <Chat
            height="40vh"
            onSendMessage={handleSendChatBotMessage}
            title="Discussion avec le bot du tour"
            submitDisabled={readyState !== ReadyState.OPEN}
            messages={botMessages}
            label="Message"
            placeholder="Qui commence le jeu?"
          />
        </Grid>
        <Grid item xs={12} md={6} xl={8}>
          <Box sx={{ height: '20rem' }}></Box>
        </Grid>
        <Grid item xs={12} md={6} xl={4}>
          <Chat
            height="24vh"
            onSendMessage={onSendGameBotMessage}
            title="Bot de jeu"
            placeholder="Entrer une instruction de jeu"
            label="Instruction de jeu"
            messages={instructions}
            submitDisabled
          />
        </Grid>
      </Grid>
    </Container>
  )
}

export default AppCoordsGen
