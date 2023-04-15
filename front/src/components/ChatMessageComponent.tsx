import { Avatar, Stack } from '@mui/material'
import ChatMessage from '../models/ChatMessage'
import { MessageAuthor } from '../models/MessageAuthor'
import RobotIcon from '@mui/icons-material/Smarttoy'
import UserIcon from '@mui/icons-material/Person'
import orange from '@mui/material/colors/orange'
import grey from '@mui/material/colors/grey'

function ChatMessageComponent({ message, author }: ChatMessage) {
  const currentUser = author == MessageAuthor.USER
  return (
    <Stack direction="row" justifyContent={currentUser ? 'flex-end' : 'unset'} sx={{ mb: 3 }}>
      {!currentUser && (
        <Avatar sx={{ width: 32, height: 32, mr: 2, bgcolor: orange[500] }}>
          <RobotIcon />
        </Avatar>
      )}
      <Stack
        sx={{
          p: 1.5,
          minWidth: 48,
          borderRadius: 1,
          overflow: 'hidden',
          typography: 'body2',
          bgcolor: orange[500],
          ...(currentUser && {
            bgcolor: grey[100],
          }),
        }}>
        {message}
      </Stack>
      {currentUser && (
        <Avatar sx={{ width: 32, height: 32, ml: 2 }}>
          <UserIcon />
        </Avatar>
      )}
    </Stack>
  )
}

export default ChatMessageComponent
