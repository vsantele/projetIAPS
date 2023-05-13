import { Box, Container, Grid, Snackbar, SnackbarCloseReason, Typography } from '@mui/material'
import useWebSocket, { ReadyState } from 'react-use-websocket'
import './App.css'
import mapImage from './assets/map.png'
import BotResponse from './models/BotResponse'
import ChatMessage from './models/ChatMessage'
import React, { MouseEvent, useEffect, useState } from 'react'
import { MessageAuthor } from './models/MessageAuthor'
import Chat from './components/Chat'
import positions from './board.json'
import PrologState from './models/PrologState'
import JsState from './models/JsState'
import { delay, teamIsBot, teamColors } from './utils'
import InfoTable from './InfoTable'
import Favicon from './assets/icon.png'

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
  playedCard: 0,
}

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

const defaultInstructions: ChatMessage[] = []

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
    playedCard: prologState.selectedCard,
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
    selectedCard: jsState.playedCard,
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
  const [instructions, setInstructions] = useState<ChatMessage[]>(defaultInstructions)

  const [gameState, setGameState] = useState(defaultState)
  const [gameIsStarted, setGameIsStarted] = useState<boolean>(false)
  const [isThinking, setIsThinking] = useState<boolean>(false)

  const [notificationMessage, setNotificationMessage] = useState<string | undefined>(undefined)

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
    message = message.trim()
    addMessageInGameChat(MessageAuthor.USER, message)

    const messageData = message.split('-')

    if (!messageData) {
      addMessageInGameChat(
        MessageAuthor.BOT,
        'Je ne comprends pas vote demande. Format requis : joueur-avance-carte (ex : 1-avance-5)'
      )
      return
    }

    const playerId = parseInt(messageData[0])
    if (playerId < 1 || playerId > 3) {
      addMessageInGameChat(
        MessageAuthor.BOT,
        'Numéro de joueur invalide. Joueurs disponibles : (1, 2, 3)'
      )
      return
    }

    const action = messageData[1]

    if (!action.includes('av') && !action.includes('forward')) {
      addMessageInGameChat(
        MessageAuthor.BOT,
        'Action de déplacement invalide (instructions autorisées : avance, forward)'
      )
      return
    }

    const card = parseInt(messageData[2])
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const currentTeam = gameState.teams.find(team => team.id == gameState.currentCountry)!
    if (!currentTeam.cards.includes(card)) {
      addMessageInGameChat(
        MessageAuthor.BOT,
        'Vous ne possédez pas la carte ' +
          card +
          '. Cartes disponibles : ' +
          [...currentTeam.cards].sort((a, b) => a - b).join('-')
      )
      return
    }

    // ToDo : call play action
    play(gameState, card)
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
      setBotMessages(defaultChatMessages)
      setInstructions(defaultInstructions)
      const response = await fetch((import.meta.env.VITE_API_HOST ?? '') + '/init')
      const data = await response.json()

      setGameIsStarted(true)
      setGameState(() => prologStateToJsState(data))
      if (teamIsBot[data.country]) {
        addMessageInGameChat(MessageAuthor.BOT, "C'est à " + data.country + ' de commencer !')
        await play(prologStateToJsState(data), 0)
      } else {
        addCurrentTeamMessageInGameBotChat(data.country)
      }
    } catch (e) {
      alert("Une erreur s'est produite lors de l'initialisation de la partie !\n" + e)
    }
  }

  const onClickNextStepButton = (event: MouseEvent<HTMLButtonElement>) => {
    play(gameState, 1)
  }

  const play = async (gameState: JsState, selectedCard: number) => {
    try {
      setIsThinking(true)
      let data = await sendMove({ ...gameState, playedCard: selectedCard })
      // if (teamIsBot[gameState.currentCountry]) {
      addMessageInGameChat(
        MessageAuthor.BOT,
        `${gameState.currentCountry} a joué ${data.selectedCard}`
      )
      // }
      setGameState(() => prologStateToJsState(data))
      if (teamIsBot[data.country]) {
        const country = data.country
        data = await sendMove(prologStateToJsState({ ...data, selectedCard: 0 }))
        await delay(100)
        addMessageInGameChat(MessageAuthor.BOT, `${country} a joué ${data.selectedCard}`)
      }
      addCurrentTeamMessageInGameBotChat(data.country)
      setGameState(() => prologStateToJsState(data))
      setIsThinking(false)
    } catch (e) {
      alert("Une erreur s'est produite lors de l'initialisation du tour !\n" + e)
    }
  }

  async function sendMove(state: JsState) {
    const prologState = jsStateToPrologState(state)
    const response = await fetch((import.meta.env.VITE_API_HOST ?? '') + '/play', {
      method: 'POST',
      body: JSON.stringify(prologState),
      headers: {
        'Content-Type': 'application/json',
      },
    })
    const data: PrologState = await response.json()
    return data
  }

  const addCurrentTeamMessageInGameBotChat = (teamName: string) => {
    addMessageInGameChat(MessageAuthor.BOT, 'Joueur actuel : ' + teamName)
  }

  const addMessageInGameChat = (author: MessageAuthor, message: string) => {
    setInstructions(instructions => [
      ...instructions,
      {
        message,
        author,
        timestamp: new Date(),
      },
    ])
  }

  const handleCloseSnackbar = (reason: SnackbarCloseReason) => {
    if (reason === 'clickaway') {
      return
    }

    setNotificationMessage(undefined)
  }

  const onClickGameHint = async () => {
    if (!gameIsStarted || teamIsBot[gameState.currentCountry]) {
      return
    }

    setNotificationMessage(undefined)

    try {
      const prologState = jsStateToPrologState(gameState)

      setNotificationMessage(
        'Je recherche la meilleure carte pour toi ! (' + gameState.currentCountry + ') ...'
      )

      const response = await fetch((import.meta.env.VITE_API_HOST ?? '') + '/bestCard', {
        method: 'POST',
        body: JSON.stringify(prologState),
        headers: {
          'Content-Type': 'application/json',
        },
      })

      if (response.status !== 200) {
        setNotificationMessage("Je suis connecté mais un soucis m'empeche de te répondre :(")
        return
      }

      const data = await response.json()

      setNotificationMessage('Je te conseille de jouer la carte ' + data.bestCard)
    } catch (e) {
      setNotificationMessage('Je ne suis pas disponible pour aider :(')
    }
  }

  return (
    <Container maxWidth={false}>
      <Box sx={{ textAlign: 'center', mb: '0.5rem' }}>
        <img src={Favicon} style={{ display: 'inline', width: '8rem', paddingRight: '0.5rem' }} />
        <small>
          {readyState ? (
            <p>Connexion avec le bot : {connectionStatus}</p>
          ) : (
            <p>Serveur du bot introuvable</p>
          )}
        </small>
        <button onClick={onClickStartGameButton}>Démarrer la partie</button>
        <button onClick={onClickNextStepButton} disabled={!gameIsStarted}>
          Prochaine étape
        </button>
        <form onSubmit={updatePos}>
          <input name="pos" />
          <button>Envoyer</button>
        </form>
      </Box>
      <Grid container justifyContent="center" alignContent="center" spacing={1}>
        <Grid item xs={12} textAlign="center">
          <div id="map-area" style={{ width: '60rem', maxWidth: '90vw', margin: 'auto' }}>
            <img
              src={mapImage}
              style={{ width: '100%' }}
              id="map-image"
              alt="Plateau de jeu tour de france"
            />
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
                      style={{
                        left: position.mapXRatio + '%',
                        top: position.mapYRatio + '%',
                        background: teamColors[team.id],
                      }}>
                      {iTeam + 1},{iPlayer + 1}
                    </div>
                  )
                }
              })
            )}
          </div>
        </Grid>
        <Grid item xs={12} md={8}>
          <Box sx={{ height: '20rem' }}>
            <InfoTable
              infos={gameState.teams}
              currentTeam={gameState.currentCountry}
              isGameStarted={gameIsStarted}
              onPlayCard={card => play(gameState, card)}
              isThinking={isThinking}
            />
          </Box>
        </Grid>
        <Grid item xs={12} md={4}>
          <Chat
            height="40vh"
            onSendMessage={onSendGameBotMessage}
            title="Bot de jeu"
            placeholder="Entrer une instruction de jeu"
            label="Instruction de jeu"
            messages={instructions}
            submitDisabled={!gameIsStarted}
            onClickHint={onClickGameHint}
          />
        </Grid>
        <Grid item xs={12}>
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
      </Grid>

      <Snackbar
        open={notificationMessage !== undefined}
        autoHideDuration={6000}
        onClose={(_, reason) => handleCloseSnackbar(reason)}
        message={notificationMessage}
      />
    </Container>
  )
}

export default App
