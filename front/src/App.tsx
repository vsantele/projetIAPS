import { Box, Container, Grid } from '@mui/material'
import useWebSocket, { ReadyState } from 'react-use-websocket'
import './App.css'
import mapImage from './assets/map.png'
import BotResponse from './models/BotResponse'
import ChatMessage from './models/ChatMessage'
import {MouseEventHandler, useEffect, useState} from 'react'
import { MessageAuthor } from './models/MessageAuthor'
import Chat from './components/Chat'
import { DataGrid, GridColDef, GridRenderCellParams } from '@mui/x-data-grid'
import positions from './board.json'

enum Team {
  ITALY = "italie",
  NETHERLANDS = "hollande",
  BELGIUM = "belgique",
  GERMANY = "allemagne",
}

const defaultTeams = [
  { id: Team.ITALY, name: 'Italie', cards: [], playersPositions: [[0, 0], [0, 0], [0, 0]]},
  { id: Team.NETHERLANDS, name: 'Pays-Bas', cards: [], playersPositions: [[0, 0], [0, 0], [0, 0]]},
  { id: Team.BELGIUM, name: 'Belgique', cards: [], playersPositions: [[0, 0], [0, 0], [0, 0]]},
  { id: Team.GERMANY, name: 'Allemange', cards: [], playersPositions: [[0, 0], [0, 0], [0, 0]]},
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

function App() {
  const [teams, setTeams] = useState(defaultTeams)
  const { sendMessage, lastMessage, readyState } = useWebSocket(
    (import.meta.env.VITE_WEBSOCKET_HOST ?? '') + '/bot',
    {
      retryOnError: true,
      reconnectInterval: 1000,
    }
  )

  const [gameIsStarted, setGameIsStarted] = useState<boolean>(false)
  const [botMessages, setBotMessages] = useState<ChatMessage[]>(defaultChatMessages)
  const [instructions, setInstruction] = useState<ChatMessage[]>(defaultInstructions)

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
    message = convertCharactersIntoRegular(message)
    message = convertPluralIntoSingular(message)
    sendMessage(JSON.stringify({ message: getAsciiValues(message.toLowerCase()) }))
  }

  const convertCharactersIntoRegular = (message: string) => {
    const letters = [
      [/á|à|â|ä/g, 'a'],
      [/é|è|ê|ë/g, 'e'],
      [/í|ì|î|ï/g, 'i'],
      [/ó|ò|ô|ö/g, 'o'],
      [/ú|ù|û|ü/g, 'u'],
      [/'|-|_/g, ' '],
      [/ç/g, 'c']
    ]

    for (let i = 0; i < letters.length; i++) {
      message = message.replace(letters[i][0], letters[i][1] as string)
    }

    return message
  }

  const convertPluralIntoSingular = (message: string) => {
    const words = [
      [' fleches', ' fleche'],
      [' rouges', ' rouge'],
      [' bleues', ' bleue'],
      [' jaunes', ' jaune'],
      [' doubles', ' double'],
      [' lettres', ' lettre'],
      [' nombres', ' nombre'],
      [' cases', ' case'],
      [' cartes', ' carte'],
      ['occupee', ' occupe'],
      ['occupe', ' occupe'],
      [' chances', ' chance'],
      [' secondes', ' seconde'],
      [' coureurs', ' coureur'],
      ['equipes', ' equipe'],
      ['equipe', ' equipe'],
      ['etapes', ' etape'],
      ['etape', ' etape'],
      [' cases', ' case'],
      [' prioritaires', ' prioritaire'],
      [' points', ' point'],
      ['accidentees', ' accidente'],
      ['accidentes', ' accidente'],
      ['accidente', ' accidente'],
      [' desavantages', ' desavantage'],
      [' avantages', ' avantage'],
      ['  ', ' '],
      ['   ', ' ']
    ]

    for (let i = 0; i < words.length; i++) {
      message = message.replace(words[i][0], words[i][1])
    }

    return message
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

  function updatePos(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()
    const formData = new FormData(e.currentTarget)
    const pos = formData.get('pos') as string
    if (pos) {
      const currentTeams = [...teams]
      const teamPos = JSON.parse(pos)

      for (let i = 0; i < teamPos.length; i++) {
        currentTeams[i].playersPositions = teamPos[i];
      }

      setTeams(currentTeams)
    }
  }

  useEffect(() => {
    console.log("Set gameIsStarted : " + gameIsStarted)

    if(!gameIsStarted){
      setTeams(defaultTeams)
      return;
    }

    fetch((import.meta.env.VITE_API_HOST ?? '') + '/init').then(response => {
      console.log(response)
    }).catch(error => alert("Une erreur s'est produite lors de l'initialisation de la partie !\n" + error))

  }, [gameIsStarted])

  const onClickStartGameButton = (event: MouseEventHandler) => {
    setGameIsStarted(true);
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
          <button onClick={onClickStartGameButton}>Démarrer la partie</button>
          <form onSubmit={updatePos}>
            <input name="pos" />
            <button>Envoyer</button>
          </form>
        </Grid>
        <Grid item xs={12} md={6} xl={8} textAlign="center">
          <div id="map-area">
            <img src={mapImage} id="map-image" alt="Plateau de jeu tour de france" />
            {teams.map((team, iTeam) =>
              team.playersPositions.map((player, iPlayer) => {
                const position = positions.find(
                  p => p.playerForward === player[0] && p.playerLateral === player[1]
                )
                if (position) {
                  return (
                    <div
                      key={player.join(',')}
                      className="player"
                      style={{ left: position.mapXRatio + '%', top: position.mapYRatio + '%' }}>
                      {iTeam + 1},{iPlayer + 1}
                    </div>
                  )
                }
              })
            )}
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

export default App
