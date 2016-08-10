shinyUI(pageWithSidebar(
  headerPanel('Melate Statistics'),
  sidebarPanel(
    'Click on the button to generate and download Excel file',
    downloadButton('downloadData', 'Download')
  ),
  mainPanel(
    h4(':)')
  )
))