import { Grid } from '@mui/material'
import ChatMessage from '../models/ChatMessage'
import { MessageAuthor } from '../models/MessageAuthor'

function ChatMessageComponent(props: ChatMessage) {
  if (props.author == MessageAuthor.USER) {
    return (
      <Grid className="chat-message" container alignItems="center">
        <Grid item xs={11}>
          <p className="text-right">{props.message}</p>
        </Grid>
        <Grid align="center" item xs={1}>
          <span className="circle">H</span>
        </Grid>
      </Grid>
    )
  }

  return (
    <Grid className="chat-message" container alignItems="center">
      <Grid align="center" item xs={1}>
        <span className="circle">B</span>
      </Grid>
      <Grid item xs={11} alignItems="center">
        <p>{props.message}</p>
      </Grid>
    </Grid>
  )
}

export default ChatMessageComponent
