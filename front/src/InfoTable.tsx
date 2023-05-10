import TeamState from './models/TeamState'
import Table from '@mui/material/Table'
import TableBody from '@mui/material/TableBody'
import TableCell from '@mui/material/TableCell'
import TableContainer from '@mui/material/TableContainer'
import TableHead from '@mui/material/TableHead'
import TableRow from '@mui/material/TableRow'
import Paper from '@mui/material/Paper'
import { teamColors, teamIsBot } from './utils'

export default function InfoTable({
  infos,
  currentTeam,
}: {
  infos: TeamState[]
  currentTeam: string
}) {
  return (
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
            <TableRow key={team.name} sx={{ '&:last-child td, &:last-child th': { border: 0 } }}>
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
                {[...team.cards].sort((a: number, b: number) => a - b).join(', ')}
              </TableCell>
              <TableCell align="left">
                {team.playersPositions.map(positions => positions.join(',')).join(' | ')}
              </TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  )
}
