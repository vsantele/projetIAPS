import Backdrop from '@mui/material/Backdrop'
import { ReactNode } from 'react'
import Loader from './Loader'

type Props = {
  open: boolean
  onClose?: () => void
  children: ReactNode
}

const BlockUi = ({ open, onClose, children }: Props) => {
  return (
    <div style={{ position: 'relative' }}>
      <Backdrop
        sx={{ color: '#FFFFFF', zIndex: theme => theme.zIndex.drawer + 1, position: 'absolute' }}
        open={open}
        onClick={() => onClose?.()}>
        <Loader show />
      </Backdrop>
      {children}
    </div>
  )
}

export default BlockUi
