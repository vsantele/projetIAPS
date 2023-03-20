import useWebSocket, { ReadyState } from 'react-use-websocket'
import './App.css'
import reactLogo from './assets/react.svg'
import BotResponse from './models/BotResponse'
import viteLogo from '/vite.svg'

function App() {
  const { sendMessage, lastMessage, readyState } = useWebSocket(
    import.meta.env.VITE_API_HOST + '/bot',
    {
      retryOnError: true,
      reconnectInterval: 1000,
    }
  )

  const connectionStatus = {
    [ReadyState.CONNECTING]: 'Connecting',
    [ReadyState.OPEN]: 'Open',
    [ReadyState.CLOSING]: 'Closing',
    [ReadyState.CLOSED]: 'Closed',
    [ReadyState.UNINSTANTIATED]: 'Uninstantiated',
  }[readyState]

  const handleClick = () => {
    sendMessage(JSON.stringify({ message: 'ping'.split(' ') }))
  }

  const parsedMessage = (res: string) => {
    const data: BotResponse = JSON.parse(res)
    return data.message.map(m => m.join(' ')).join('\n')
  }

  return (
    <div className="App">
      <div>
        <a href="https://vitejs.dev" target="_blank">
          <img src={viteLogo} className="logo" alt="Vite logo" />
        </a>
        <a href="https://reactjs.org" target="_blank">
          <img src={reactLogo} className="logo react" alt="React logo" />
        </a>
      </div>
      <h1>Vite + React</h1>
      <div className="card">
        {readyState && <p>readyState: {connectionStatus}</p>}
        <button onClick={handleClick} disabled={readyState !== ReadyState.OPEN}>
          Send Message
        </button>
        <div>{lastMessage && <p>lastMessage: {parsedMessage(lastMessage.data)}</p>}</div>
      </div>
    </div>
  )
}

export default App
