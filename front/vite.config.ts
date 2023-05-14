import react from '@vitejs/plugin-react-swc'
import { defineConfig } from 'vite'
import { VitePWA } from 'vite-plugin-pwa'
// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react(), VitePWA({
    injectRegister: 'auto',
    manifestFilename: 'manifest.json',
    registerType: "prompt",
    minify: true,
    manifest: {
      "name": "Projet IAPS - Tour de france",
      "short_name": "Projet IAPS",
      "start_url": "/",
      "display": "standalone",
      "description": "Projet IAPS - Tour de france",
      "lang": "fr",
      "dir": "ltr",
      "theme_color": "#000000",
      "background_color": "#000000",
      "orientation": "any",
      "icons": [
        {
          "src": "/favicon/manifest-icon-192.maskable.png",
          "sizes": "192x192",
          "type": "image/png",
          "purpose": "any"
        },
        {
          "src": "/favicon/manifest-icon-192.maskable.png",
          "sizes": "192x192",
          "type": "image/png",
          "purpose": "maskable"
        },
        {
          "src": "/favicon/manifest-icon-512.maskable.png",
          "sizes": "512x512",
          "type": "image/png",
          "purpose": "any"
        },
        {
          "src": "/favicon/manifest-icon-512.maskable.png",
          "sizes": "512x512",
          "type": "image/png",
          "purpose": "maskable"
        },
        {
          "src": "/favicon/Square44x44Logo.png",
          "sizes": "44x44",
          "type": "image/png",
          "purpose": "any maskable"
        },
        {
          "src": "/favicon/Square50x50Logo.png",
          "sizes": "50x50",
          "type": "image/png",
          "purpose": "any maskable"
        },
        {
          "src": "/favicon/Square150x150Logo.png",
          "sizes": "150x150",
          "type": "image/png",
          "purpose": "any maskable"
        },
        {
          "src": "/favicon/Square44x44Logo.png",
          "sizes": "44x44",
          "type": "image/png",
          "purpose": "maskable"
        },
        {
          "src": "/favicon/Square50x50Logo.png",
          "sizes": "50x50",
          "type": "image/png",
          "purpose": "maskable"
        },
        {
          "src": "/favicon/Square150x150Logo.png",
          "sizes": "150x150",
          "type": "image/png",
          "purpose": "maskable"
        }
      ],
      "screenshots": [
        {
          "src": "/screenshots/1280x800-screenshot.png",
          "sizes": "1280x800",
          "type": "image/png"
        },
        {
          "src": "/screenshots/750x1334-screenshot.png",
          "sizes": "750x1334",
          "type": "image/png"
        }
      ],
      "prefer_related_applications": false,
      "shortcuts": [
        {
          "name": "Projet IAPS - Tour de france",
          "short_name": "Projet IAPS",
          "description": "Projet IAPS - Tour de france",
          "url": "/",
          "icons": [
            {
              "src": "/favicon/manifest-icon-512.maskable.png",
              "sizes": "512x512",
              "type": "image/png"
            }
          ]
        }
      ]
    }
  })],
  build: {
    outDir: '../dist',
  },
})
