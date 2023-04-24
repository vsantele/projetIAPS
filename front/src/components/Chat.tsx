import React, { useEffect, useRef, useState } from 'react'
import { Button, Grid, IconButton, TextField } from '@mui/material'
import ChatMessageComponent from './ChatMessageComponent'
import ChatMessage from '../models/ChatMessage'
import SendIcon from '@mui/icons-material/Send'

interface ChatProps {
  onSendMessage(message: string): void
  submitDisabled?: boolean
  title: string
  placeholder?: string
  label?: string
  height?: string
  messages: ChatMessage[]
}

const Chat = ({
  onSendMessage,
  submitDisabled = false,
  title,
  placeholder,
  label,
  height = '100%',
  messages,
}: ChatProps) => {
  // User message
  const [userMessage, setUserMessage] = useState<string>('')

  const messagesEndRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    if (messagesEndRef.current) {
      messagesEndRef.current.scrollTop = messagesEndRef.current.scrollHeight
    }
  }, [messages])

  const handleSend = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Enter' && userMessage !== '') {
      sendMessage()
    }
  }

  const sendMessage = () => {
    onSendMessage(userMessage)
    setUserMessage('')
    if (messagesEndRef.current) {
      messagesEndRef.current.scrollTop = messagesEndRef.current.scrollHeight
    }
  }

  return (
    <Grid container>
      <Grid item xs={12} sx={{ m: 0, p: 0 }}>
        <h2 className="text-center">{title}</h2>
      </Grid>

      <Grid
        item
        style={{ overflowY: 'scroll', width: '100%' }}
        height={height}
        ref={messagesEndRef}>
        {messages.map(chatMessage => {
          return <ChatMessageComponent {...chatMessage} key={chatMessage.timestamp.getTime()} />
        })}
      </Grid>

      <Grid container spacing={2} alignItems="center" sx={{ px: 3, my: 0 }}>
        <Grid item xs={11} sx={{ m: 0, p: 0 }}>
          <TextField
            size="small"
            label={label ?? 'Message'}
            placeholder={placeholder ?? ''}
            variant="filled"
            fullWidth
            value={userMessage}
            onKeyUp={handleSend}
            onChange={e => setUserMessage(e.target.value ?? '')}
            disabled={submitDisabled}
          />
        </Grid>
        <Grid item xs={1}>
          {/* <Button
            variant="contained"
            onClick={sendMessage}
            disabled={submitDisabled || userMessage === ''}
            endIcon={<SendIcon />}>
            Envoyer
          </Button> */}
          <IconButton onClick={sendMessage} disabled={submitDisabled || userMessage === ''}>
            <SendIcon />
          </IconButton>
        </Grid>
      </Grid>
    </Grid>
  )
}

export default Chat
