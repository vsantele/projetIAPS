import TeamState from './models/TeamState'
import Table from '@mui/material/Table'
import TableBody from '@mui/material/TableBody'
import TableCell from '@mui/material/TableCell'
import TableContainer from '@mui/material/TableContainer'
import TableHead from '@mui/material/TableHead'
import TableRow from '@mui/material/TableRow'
import Paper from '@mui/material/Paper'
import Button from '@mui/material/Button'
import { teamColors, teamIsBot } from './utils'
import ButtonGroup from '@mui/material/ButtonGroup'
import BlockUi from './components/BlockUi'

function convertCoord([x, y]: number[]) {
  if (x % 10 === 0) {
    const xClean = Math.floor(x / 10)
    return `${xClean},${y}`
  }
  const xCleqn = Math.floor(x / 10)
  const letter = String.fromCharCode(97 + (x % 10) - 1)
  return `${xCleqn}${letter},${y}`
}

export default function InfoTable({
  infos,
  currentTeam,
  isGameStarted,
  isThinking,
  onPlayCard,
}: {
  infos: TeamState[]
  currentTeam: string
  isGameStarted: boolean
  isThinking: boolean
  onPlayCard: (card: number) => void
}) {
  return (
    <BlockUi open={isThinking}>
      <TableContainer component={Paper}>
        <Table aria-label="Teams informations">
          <TableHead>
            <TableRow>
              <TableCell></TableCell>
              <TableCell>Equipes</TableCell>
              <TableCell align="left">Cartes</TableCell>
              <TableCell align="left">Positions</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {infos.map(team => (
              <TableRow
                key={team.name}
                sx={{
                  '&:last-child td, &:last-child th': { border: 0 },
                  backgroundColor:
                    isGameStarted && currentTeam === team.id ? 'lightgrey' : 'inherit',
                }}>
                <TableCell component="th" scope="row">
                  <div
                    style={{
                      backgroundColor: teamColors[team.id],
                      width: '1rem',
                      height: '1rem',
                      borderRadius: '50%',
                    }}></div>
                </TableCell>
                <TableCell component="th" scope="row">
                  {team.name}
                </TableCell>
                <TableCell align="left">
                  <ShowCards
                    cards={team.cards}
                    disabled={teamIsBot[team.id] || currentTeam !== team.id}
                    onPlayCard={onPlayCard}
                  />
                </TableCell>
                <TableCell align="left">
                  {team.playersPositions.map(positions => convertCoord(positions)).join(' | ')}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
    </BlockUi>
  )
}

function ShowCards({
  cards,
  disabled,
  onPlayCard,
}: {
  cards: number[]
  disabled: boolean
  onPlayCard: (card: number) => void
}) {
  return (
    <ButtonGroup variant="text" aria-label="outlined primary button group">
      {[...cards]
        .sort((a: number, b: number) => a - b)
        .map((card, index) => (
          <Button key={index} disabled={disabled} onClick={() => onPlayCard(card)}>
            {card}
          </Button>
        ))}
    </ButtonGroup>
  )
}
