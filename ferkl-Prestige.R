library(carData)
data("Prestige")
summary(Prestige)

GGally::ggpairs(Prestige)

m_1 <- lm(prestige ~ ., data = Prestige)
summary(m_1)

m_2 <- update(m_1 ,~ . - women, data = Prestige)
summary(m_2)

m_3 <- update(m_2 ,~ . - census, data = Prestige)
summary(m_3)

m_4 <- update(m_3 ,~ . - type, data = Prestige)
summary(m_4)
