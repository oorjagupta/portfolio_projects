--CHECKING TABLES
Select *
From port..CovidDeaths
where continent is not null
Order by 3, 4

Select *
From port..CovidVaccinations
Order by 3, 4

-- EXTRACTING DATA

Select continent, location, date, total_cases, new_cases, total_deaths, population
From port..CovidDeaths
where continent is not null
order by 1, 2 

-- TOTAL CASES VS TOTAL DEATHS
-- LIKELYHOOD OF DYING OF COVID

Select continent, location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
From port..CovidDeaths
 Where continent is not null and location like '%India'
order by 1, 2 

--TOTAL CASES VS POPULATION - INDIA

Select continent, location, date, population, total_cases, (total_cases/population)*100 as PercentPoplationInfected
From port..CovidDeaths
Where continent is not null and location like '%India'
order by 1, 2 

--HIGHEST INFECTED RATE VS POPULATION

Select continent, location, population, MAX(total_cases) as HighestInfectionCount, MAX((total_cases/population))*100 
as PercentPopulationInfected
From port..CovidDeaths
where continent is not null
Group by continent, location, population
order by 4 desc 

--HIGHEST DEATH COUNT PER POPULATION

Select location, MAX(cast(total_deaths as int)) as TotalDeathCount
From port..CovidDeaths
where continent is not null
Group by location
order by TotalDeathCount desc 


-- BASED ON CONTINENT
-- CONTINENTS WITH HIGHEST DEATH COUNT

Select continent, MAX(cast(total_deaths as int)) as TotalDeathCount
From port..CovidDeaths
where continent is not null
Group by continent
order by 2 desc

-- COVID CASES WORLDWIDE

Select SUM(total_cases) as Totalcases, SUM(cast(new_deaths as int)) as TotalDeaths, SUM(cast(new_deaths as int))/
SUM(new_cases)*100 as DeathPercentage
From port..CovidDeaths
where continent is not null
order by 1, 2 

--VACCINATION

Select *
From port..CovidDeaths dea
Join port.. CovidVaccinations vac
on dea.location = vac.location
and dea.date = vac.date

--TOTAL POPULATION VS VACCINATION

Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(cast(vac.new_vaccinations as int)) OVER (Partition by dea.location ORDER by dea.location,
dea.date) as RollingPeopleVaccinated
 --(RollingPeopleVaccinated/population)*100 
FROM port..CovidDeaths dea
Join port.. CovidVaccinations vac
   on dea.location = vac.location
   and dea.date = vac.date
where dea.continent is not null
order by 2,3


--USE CTE

with PopvsVac (Continent, Location, Date, Population, new_vaccinations, TotalVaccinated)
as
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(CONVERT(int, vac.new_vaccinations)) OVER (Partition by dea.location ORDER by dea.location,
dea.date) as TotalVaccinated
From port..CovidDeaths dea
Join port.. CovidVaccinations vac
   on dea.location = vac.location
   and dea.date = vac.date
where dea.continent is not null
--order by 2, 3
)
Select *, (TotalVaccinated/population)*100
From PopvsVac

--CREATING TEMP TABLE

Drop Table if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric, 
New_vaccinations numeric,
TotalVaccinated numeric
)
Insert Into #PercentPopulationVaccinated
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(CONVERT(int, vac.new_vaccinations)) OVER (Partition by dea.location ORDER by dea.location,
dea.date) as TotalVaccinated
From port..CovidDeaths dea
Join port.. CovidVaccinations vac
   on dea.location = vac.location
   and dea.date = vac.date
where dea.continent is not null

Select *, (TotalVaccinated/population)*100
From #PercentPopulationVaccinated


--CREATING VIEW FOR VISUALIZATION

CREATE VIEW PercentPopulationVaccinated as
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, SUM(CONVERT(int, vac.new_vaccinations)) OVER (Partition by dea.location ORDER by dea.location,
dea.date) as TotalVaccinated
From port..CovidDeaths dea
Join port.. CovidVaccinations vac
   on dea.location = vac.location
   and dea.date = vac.date
where dea.continent is not null

SELECT *
FROM PercentPopulationVaccinated

