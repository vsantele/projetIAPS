import classes from './Loader.module.css'

export default function Loader({ show }: { show: boolean }) {
  return show ? <div className={classes['custom-loader']}></div> : null
}
