import { useState } from 'react'
import { Button, Grid, TextField } from '@mui/material'
import ChatMessageComponent from './ChatMessageComponent'
import ChatMessage from '../models/ChatMessage'

interface ChatProps {
  onSendMessage(message: string): void
  submitDisabled?: boolean
  title: string
  placeholder?: string
  height?: string
  messages: ChatMessage[]
}

const Chat = ({
  onSendMessage,
  submitDisabled = false,
  title,
  placeholder,
  height = '100%',
  messages,
}: ChatProps) => {
  // User message
  const [userMessage, setUserMessage] = useState<string>('')

  const sendMessageClick = () => {
    onSendMessage(userMessage)
    setUserMessage('')
  }

  return (
    <Grid container>
      <Grid item xs={12} sx={{ m: 0, p: 0 }}>
        <h2 className="text-center">{title}</h2>
      </Grid>

      <Grid item style={{ overflowY: 'scroll' }} height={height}>
        {messages.map(chatMessage => {
          return <ChatMessageComponent {...chatMessage} key={chatMessage.timestamp.getTime()} />
        })}
      </Grid>

      <Grid container spacing={2} alignItems="center" sx={{ px: 3, my: 0 }}>
        <Grid item xs={10} sx={{ m: 0, p: 0 }}>
          <TextField
            size="small"
            label={placeholder ?? 'Entrer votre message ...'}
            variant="filled"
            fullWidth
            value={userMessage}
            onChange={e => setUserMessage(e.target.value ?? '')}
          />
        </Grid>
        <Grid item xs={2}>
          <Button
            variant="contained"
            onClick={sendMessageClick}
            disabled={submitDisabled || userMessage === ''}
            fullWidth>
            Envoyer
          </Button>
        </Grid>
      </Grid>
    </Grid>
  )
}

export default Chat
