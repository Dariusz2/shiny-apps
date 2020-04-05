library(shiny)

ui <- fluidPage(
    checkboxGroupInput(inputId = "wybrane", label = "Wybierz spółki z WIG20 do wyliczenia korelacji",
                       choices = c("WIG20", "ALIOR", "CCC", "CDPROJEKT", "CYFRPLSAT", "DINOPL", "JSW", "KGHM",
                                   "LPP", "LOTOS", "MBANK", "ORANGEPL", "PEKAO", "PGE", "PGNIG", "PKNORLEN",
                                   "PKOBP", "PLAY", "PZU", "SANPL", "TAURONPE"),
                       selected = c("WIG20", "ALIOR", "CCC", "CDPROJEKT", "CYFRPLSAT", "DINOPL", "JSW", "KGHM",
                                    "LPP", "LOTOS", "MBANK", "ORANGEPL", "PEKAO", "PGE", "PGNIG", "PKNORLEN",
                                    "PKOBP", "PLAY", "PZU", "SANPL", "TAURONPE")),
    plotOutput("korel")
)

server <- function(input, output) {
    output$korel <- renderPlot(
        {
            
            
            library(tidyverse)
            library(corrplot)
            
            # options(digits = 4)
            
            podokres <- 400 # liczba dni do badania okna
            
            skroty <- c("WIG20", "ALR", "CCC", "CDR", "CPS", "DNP", "JSW", "KGH", "LPP", "LTS",
                        "MBK", "OPL", "PEO", "PGE", "PGN", "PKN", "PKO", "PLY", "PZU",
                        "SPL", "TPE")
            
            nazwy <- c("WIG20", "ALIOR", "CCC", "CDPROJEKT", "CYFRPLSAT", "DINOPL", "JSW", "KGHM", "LPP",
                       "LOTOS", "MBANK", "ORANGEPL", "PEKAO", "PGE", "PGNIG", "PKNORLEN", "PKOBP",
                       "PLAY", "PZU", "SANPL", "TAURONPE")
            
            adresy <- paste0("https://stooq.pl/q/d/l/?s=", skroty, "&i=d")
            
            
            # Pobranie danych
            pobierz_dane <- function() {
                for (i in seq(length(nazwy))) {
                    assign(nazwy[i], add_column(read_csv(adresy[i]), Spolka = nazwy[i], .before = TRUE))
                }
                
                # Połączenie w jedną tabelę
                koszyk <- get(nazwy[1])
                for (i in 2:length(nazwy)) {
                    koszyk <- bind_rows(koszyk, get(nazwy[i]))
                }
                
                koszyk <- as_tibble(koszyk)
                
                # Przekształcenie dat
                koszyk <- koszyk %>%
                    mutate(Data2 = as.Date(Data, format = "%Y-%m-%d")) %>%
                    select(-Data) %>%
                    select(Data2, everything()) %>%
                    rename(Data = Data2)
                
            }
            
            # Nazwa pliku z danymi
            nazwa_pliku <- paste0("Koszyk_", as.character(format(Sys.Date(), "%Y_%m_%d")), ".csv")
            
            # Pobranie danych, jeśli 
            if (exists("koszyk")) {
                print("OK")
            } else {
                if (file.exists(nazwa_pliku)) {
                    print("dane dzisiaj pobrane")
                } else {
                    unlink("*.csv")
                    pobierz_dane()
                    write_csv(koszyk, paste0(nazwa_pliku))
                }
            }
            
            # Wybór podokresu
            krotka <- filter(koszyk, Data > Sys.Date() - podokres)
            
            # Skrocenie tabeli do spolek wybranych przez uzytkownika
            krotka <- krotka %>%
                select(Spolka, Data, Zamkniecie) %>%
                filter(Spolka %in% input$wybrane)
            
            # Transpozycja kolumn
            szeregi <- spread(krotka, key = Spolka, value = Zamkniecie)
            
            # Wybor tylko "wybranych" - wskazanych przez uzytkownika Shiny - i wyliczenie korelacji
            szeregi %>%
                select(-Data) %>%
                cor() -> M

            # Utworzenie wykresu
            corrplot(M, method = "pie")
        }
    )
}

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)