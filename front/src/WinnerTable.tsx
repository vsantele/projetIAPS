import {
  TableContainer,
  Paper,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
} from '@mui/material'
import { Winner } from './models/WinnerResult'
import { teamColors, teamIsBot } from './utils'
import JsState from './models/JsState'

export default function winnerTable({ winner, state }: { winner: Winner; state: JsState }) {
  return (
    <TableContainer component={Paper}>
      <Table aria-label="Teams informations">
        <TableHead>
          <TableRow>
            <TableCell></TableCell>
            <TableCell>Equipes</TableCell>
            <TableCell align="left">Joueur 1</TableCell>
            <TableCell align="left">Joueur 2</TableCell>
            <TableCell align="left">Joueur 3</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {state.teams.map((team, i) => (
            <TableRow
              key={team.name}
              sx={{
                '&:last-child td, &:last-child th': { border: 0 },
                backgroundColor: team.name === winner.winnerTeam ? 'lightgrey' : 'inherit',
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
              {winner.ranking[i].map((player, j) => (
                <TableCell align="left" key={j}>
                  {player}
                </TableCell>
              ))}
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  )
}
