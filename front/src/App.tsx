import { Box, Container, Grid } from '@mui/material'
import useWebSocket, { ReadyState } from 'react-use-websocket'
import './App.css'
import mapImage from './assets/map.png'
import BotResponse from './models/BotResponse'
import ChatMessage from './models/ChatMessage'
import { MouseEvent, useEffect, useState } from 'react'
import { MessageAuthor } from './models/MessageAuthor'
import Chat from './components/Chat'
import { DataGrid, GridColDef, GridRenderCellParams } from '@mui/x-data-grid'
import positions from './board.json'
import PrologState from './models/PrologState'
import JsState from './models/JsState'

const defaultState: JsState = {
  currentCountry: 'italie',
  cards: [],
  teams: [
    {
      id: 'italie',
      name: 'Italie',
      cards: [],
      playersPositions: [
        [0, 0],
        [0, 0],
        [0, 0],
      ],
    },
    {
      id: 'hollande',
      name: 'Pays-Bas',
      cards: [],
      playersPositions: [
        [0, 0],
        [0, 0],
        [0, 0],
      ],
    },
    {
      id: 'belgique',
      name: 'Belgique',
      cards: [],
      playersPositions: [
        [0, 0],
        [0, 0],
        [0, 0],
      ],
    },
    {
      id: 'allemagne',
      name: 'Allemange',
      cards: [],
      playersPositions: [
        [0, 0],
        [0, 0],
        [0, 0],
      ],
    },
  ],
}

const teamsGridColumns: GridColDef[] = [
  { headerName: 'Équipe', field: 'name', resizable: false, flex: 1 },
  {
    headerName: 'Cartes',
    field: 'cards',
    resizable: false,
    flex: 2,
    renderCell: (params: GridRenderCellParams<number[]>) => <p>{params.value.join(' - ')}</p>,
  },
  {
    headerName: 'Positions',
    field: 'playersPositions',
    flex: 3,
    renderCell: (params: GridRenderCellParams<number[]>) => <p>{params.value.join(' | ')}</p>,
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

const convertCharactersIntoRegular = (message: string) => {
  const letters = [
    [/á|à|â|ä/g, 'a'],
    [/é|è|ê|ë/g, 'e'],
    [/í|ì|î|ï/g, 'i'],
    [/ó|ò|ô|ö/g, 'o'],
    [/ú|ù|û|ü/g, 'u'],
    [/'|-|_/g, ' '],
    [/ç/g, 'c'],
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
    ['   ', ' '],
  ]

  for (let i = 0; i < words.length; i++) {
    message = message.replace(words[i][0], words[i][1])
  }

  return message
}

function prologStateToJsState(prologState: PrologState): JsState {
  const jsState: JsState = {
    currentCountry: prologState.country,
    cards: prologState.cards,
    teams: [],
  }

  for (let i = 0; i < defaultState.teams.length; i++) {
    jsState.teams.push({
      id: defaultState.teams[i].id,
      name: defaultState.teams[i].name,
      cards: prologState.countriesCards[i],
      playersPositions: prologState.playersPositions[i],
    })
  }

  return jsState
}

function jsStateToPrologState(jsState: JsState): PrologState {
  return {
    cards: jsState.cards,
    countriesCards: jsState.teams.map(team => team.cards),
    country: jsState.currentCountry,
    playersPositions: jsState.teams.map(team => team.playersPositions),
  }
}

function App() {
  const { sendMessage, lastMessage, readyState } = useWebSocket(
    (import.meta.env.VITE_WEBSOCKET_HOST ?? '') + '/bot',
    {
      retryOnError: true,
      reconnectInterval: 1000,
    }
  )

  const [botMessages, setBotMessages] = useState<ChatMessage[]>(defaultChatMessages)
  const [instructions, setInstruction] = useState<ChatMessage[]>(defaultInstructions)

  const [gameState, setGameState] = useState(defaultState)
  const [gameIsStarted, setGameIsStarted] = useState<boolean>(false)

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
      const currentState = { ...gameState }
      const teamPos = JSON.parse(pos)

      for (let i = 0; i < teamPos.length; i++) {
        currentState.teams[i].playersPositions = teamPos[i]
      }

      setGameState(currentState)
    }
  }

  const onClickStartGameButton = async (event: MouseEvent<HTMLButtonElement>) => {
    try {
      const response = await fetch((import.meta.env.VITE_API_HOST ?? '') + '/init')
      const data = await response.json()

      setGameState(prologStateToJsState(data))
      setGameIsStarted(true)
    } catch (e) {
      alert("Une erreur s'est produite lors de l'initialisation de la partie !\n" + e)
    }
  }

  const onClickNextStepButton = (event: MouseEvent<HTMLButtonElement>) => {
    play(1)
  }

  const play = async (selectedCard: number) => {
    try {
      const prologState = jsStateToPrologState(gameState)
      console.log(prologState)

      const response = await fetch((import.meta.env.VITE_API_HOST ?? '') + '/play', {
        method: 'POST',
        body: JSON.stringify(prologState),
        headers: {
          'Content-Type': 'application/json',
        },
      })

      const data: PrologState = await response.json()
      setGameState(prologStateToJsState(data))
    } catch (e) {
      alert("Une erreur s'est produite lors de l'initialisation du tour !\n" + e)
    }
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
          <button onClick={onClickStartGameButton} disabled={gameIsStarted}>
            Démarrer la partie
          </button>
          <button onClick={onClickNextStepButton} disabled={!gameIsStarted}>
            Prochaine étape
          </button>
          <form onSubmit={updatePos}>
            <input name="pos" />
            <button>Envoyer</button>
          </form>
        </Grid>
        <Grid item xs={12} md={6} xl={8} textAlign="center">
          <div id="map-area">
            <img src={mapImage} id="map-image" alt="Plateau de jeu tour de france" />
            {gameState.teams.map((team, iTeam) =>
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
              rows={gameState.teams}
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
            submitDisabled={!gameIsStarted}
          />
        </Grid>
      </Grid>
    </Container>
  )
}

export default App
