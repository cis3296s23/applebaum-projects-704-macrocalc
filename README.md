[![Open in Visual Studio Code](https://classroom.github.com/assets/open-in-vscode-c66648af7eb3fe8bc4f294546bfd86ef473780cde1dea487d3c4ff354943c9ae.svg)](https://classroom.github.com/online_ide?assignment_repo_id=10609743&assignment_repo_type=AssignmentRepo)
<div align="center">

# MacroCalc
[![Report Issue on Jira](https://img.shields.io/badge/Report%20Issues-Jira-0052CC?style=flat&logo=jira-software)](https://temple-cis-projects-in-cs.atlassian.net/jira/software/c/projects/DT/issues)
[![Deploy Docs](https://github.com/ApplebaumIan/tu-cis-4398-docs-template/actions/workflows/deploy.yml/badge.svg)](https://github.com/ApplebaumIan/tu-cis-4398-docs-template/actions/workflows/deploy.yml)
[![Documentation Website Link](https://img.shields.io/badge/-Documentation%20Website-brightgreen)](https://applebaumian.github.io/tu-cis-4398-docs-template/)


</div>

## Project Abstract

This application aims to provide individuals with a tool to calculate the nutrient breakdown of any recipe. Users can give the application a recipe with ingredients and their amounts which will then be converted to its nutrient breakdown per serving based on values from an existing database. Additionally, meals can be saved and modified and compared to each other. User meals can be logged and compared against a preset macronutrient limit to help users reach their goals.

## Conceptual Design

![image](https://user-images.githubusercontent.com/89528532/232869621-f22f5617-d928-4821-b4fb-309d4c3592b8.png)

The project will be an application with a search tool for selecting ingredients as well as an entry field to submit full recipes. Depending on the selected ingredients a visual demonstration will display the nutrient amounts compared against your personal daily limit. This project will use the R programming language (specifically the Shiny package) and python. The Shiny package is for constructing interactive web applications from R with easy-to-understand graphics such (bar graphs, charts, etc. Python will be used to create and store user profiles or allow sign-ins through other methods as this is not available through R. Code from the open source will need to be moved and reformatted so that a proper menu and tabs can be created rather than having all the information on one page.

## Background

MacroCalc will function as a nutrition planning web application. It will allow users to upload recipes or individual ingredients and in return receive a detailed nutrient breakdown. These meals can be saved, modified, and compared to each other. A user’s meals will be compared against their preset macronutrient goals. The application will be fully functional on and offline and allows for customization and conversion in terms of amounts.

## Required Resources

To complete this project, we will need to find a large database of ingredients and their nutrient values I can use as a base data source in addition to personal ingredients that a user may add. These are publicly available on government funded resource pages. In terms of software resources, I will just need RStudio to make use of the R programming language and an installation of the Shiny package. No specific hardware resource will be needed other than a device that can utilize RStudio.

This is similar to paid closed-source nutrition tools that already exist such as Noom or MyFitnessPal. Unlike these tools however, MacroCalc will have a focus on offline usability, quantity customization, and simple design to help users have greater freedom and accessibility in achieving their personal nutrition goals.

## Collaborators

[//]: # ( readme: collaborators -start )
<table>
<tr>
    <td align="center">
        <a href="https://github.com/son2005">
            <img src="https://avatars.githubusercontent.com/u/18011568?v=4" width="100;" alt="ApplebaumIan"/>
            <br />
            <sub><b>Son Tran</b></sub>
        </a>
    </td>
    <td align="center">
        <a href="https://github.com/jutobash">
            <img src="https://avatars.githubusercontent.com/u/89528532?v=4" width="100;" alt="ApplebaumIan"/>
            <br />
            <sub><b>Justin Kuruvilla</b></sub>
        </a>
    </td>
    <td align="center">
        <a href="https://github.com/PatrickBrady7">
            <img src="https://avatars.githubusercontent.com/u/97626904?v=4" width="100;" alt="ApplebaumIan"/>
            <br />
            <sub><b>Patrick Brady</b></sub>
        </a>
    </td>
    <td align="center">
        <a href="https://github.com/gevdram">
            <img src="https://avatars.githubusercontent.com/u/111989879?v=4" width="100;" alt="leighflagg"/>
            <br />
            <sub><b>Gevork Dramagotchian</b></sub>
        </a>
    </td></tr>
</table>

[//]: # ( readme: collaborators -end )
