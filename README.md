# Сначала надо поставить stack
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

# Сборка
```bash

git clone git@ gitlab. com: tempe/shtuki. git
cd shtuki

# Требуется при пересборке
stack install --local-bin-path bin/
```

# Запуск

```bash
echo NEOPASSWORD=yourpassword > .env
docker-compose up

# Пересборка - перезапуск
stack install --local-bin-path bin/ && docker-compose restart
```

# Доступ к базе

Браузером на localhost:7474 neo4j:yourpassowrd

Дока по языку запросов: https://neo4j.com/docs/developer-manual/current/cypher/

# Api

Описано на серванте. Живет в src/Api.hs. 


