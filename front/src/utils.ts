export async function delay(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export const teamColors: Record<string, string> = {
  italie: '#ff00ff',
  hollande: '#fff900',
  belgique: '#00ff3c',
  allemagne: '#ff0000',
}

export const teamIsBot: Record<string, boolean> = {
  italie: false,
  hollande: true,
  belgique: false,
  allemagne: true,
}