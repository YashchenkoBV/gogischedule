# GoGiSchedule

## Brief Description
GoGiSchedule is a Prolog-based scheduling system designed to generate and optimize schedules for educational institutions. Leveraging constraint logic programming (CLP(FD)), it efficiently manages scheduling constraints and provides a web interface for data entry, schedule generation, and schedule visualization.

## Prerequisites
- **SWI-Prolog** (version 8.4 or later recommended)
- Prolog libraries:
  - `clpfd`
  - `http/thread_httpd`
  - `http/http_dispatch`
  - `http/html_write`
  - `http/http_parameters`
  - `http/http_files`

## Launching Instructions
1. **Clone the Repository:**
   ```bash
   git clone <https://github.com/CSAI-programming-paradigms/prolog-project-2025-YashchenkoBV.git>
   cd prolog-project-2025-YashchenkoBV

   ```

2. **Start the Prolog Interpreter:**
   ```bash
   swipl
   ```

3. **Load the Project Files:**
   ```prolog
   ?- [launch].
   ?- help.
   ```

4. **Start the Server:**
   - For interactive mode (choose between an empty database or test data):
     ```prolog
     ?- start.
     ** follow the prompts ** 
     ```

5. **Access the Application:**
   Open your web browser and navigate to:
   ```
   http://localhost:8000/
   ```

6. **Stop the Server:**
   To stop the running server, execute:
   ```prolog
   ?- halt.
   ```

## Web Application Routes
The GoGiSchedule web server defines the following routes:

- **Home Page:** `/`  
  Displays the welcome page and navigation menu.

- **Data Input Page:** `/input`  
  Provides forms to add new teachers, rooms, groups, and subjects.

- **Process Endpoints:**
  - `/process/teacher` – Processes teacher addition.
  - `/process/room` – Processes room addition.
  - `/process/group` – Processes group addition.
  - `/process/subject` – Processes subject addition.

- **Display Data:** `/display`  
  Shows all current data entries (teachers, rooms, groups, and subjects).

- **Schedule Generation:** `/schedule`  
  Initiates the scheduling algorithm to generate and optimize a schedule.

- **Schedule Views:**
  - `/view_schedule` – Displays the full schedule in a matrix view.
  - `/teacher_schedule` – Displays the schedule filtered by teacher.
  - `/group_schedule` – Displays the schedule filtered by group.
  - `/room_schedule` – Displays the schedule filtered by room.

- **Schedule Export:** `/export`  
  Exports the generated schedule as an HTML file.

- **Static Files:** `/static` (with prefix)  
  Serves static assets (e.g., CSS files) required by the web interface.
