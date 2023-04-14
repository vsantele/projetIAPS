import { MessageAuthor } from './MessageAuthor'

export default interface ChatMessage {
  message: string
  timestamp: Date
  author: MessageAuthor
}
