import { Box, Container, Grid } from '@mui/material'
import useWebSocket, { ReadyState } from 'react-use-websocket'
import './App.css'
import mapImage from './assets/map.png'
import BotResponse from './models/BotResponse'
import ChatMessage from './models/ChatMessage'
import { useEffect, useState } from 'react'
import { MessageAuthor } from './models/MessageAuthor'
import Chat from './components/Chat'
import { DataGrid, GridColDef, GridRenderCellParams } from '@mui/x-data-grid'

enum Team {
  ITALY,
  NETHERLANDS,
  BELGIUM,
  GERMANY,
}

interface Vector2 {
  x: number,
  y: number
}

const teams = [
  { id: Team.ITALY, name: 'Italie', cards: [1, 2, 4, 8, 12] },
  { id: Team.NETHERLANDS, name: 'Pays-Bas', cards: [5, 7, 10, 11, 12] },
  { id: Team.BELGIUM, name: 'Belgique', cards: [3, 5, 7, 9, 12] },
  { id: Team.GERMANY, name: 'Allemange', cards: [1, 6, 7, 8, 10] },
]

const teamsGridColumns: GridColDef[] = [
  { headerName: 'Équipe', field: 'name', resizable: false, flex: 1 },
  {
    headerName: 'Cartes',
    field: 'cards',
    resizable: false,
    flex: 3,
    renderCell: (params: GridRenderCellParams<number[]>) => <p>{params.value.join(' - ')}</p>,
  },
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
    (import.meta.env.VITE_API_HOST ?? '') + '/bot',
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

  const [vectors, setVectors] = useState<Vector2[]>([])

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

  const onClickImage = (event) => {
    const elem = document.getElementById("map-area");//outer starts at your elem then walks out
    const bounding = elem.getBoundingClientRect();
    const playerImageSize = 10;

    const xRatio = (((event.clientX - (playerImageSize / 2)) - bounding.left) / bounding.width) * 100;
    const yRatio = (((event.clientY - (playerImageSize / 2)) - bounding.top) / bounding.height) * 100;

    console.log(xRatio + "%, " + yRatio + "%")

    setVectors(vectors => [...vectors, {x : xRatio, y: yRatio}]);
  }

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
              vectors.map(vector => {
                return <div className="player" style={{left: vector.x + "%", top: vector.y + "%"}}/>
              })
            }
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
          <Box sx={{ height: '20rem' }}>
            <DataGrid
              columns={teamsGridColumns}
              rows={teams}
              rowSelection={false}
              disableRowSelectionOnClick
              hideFooter={true}
              hideFooterPagination={true}
              hideFooterSelectedRowCount={true}
            />
          </Box>
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
