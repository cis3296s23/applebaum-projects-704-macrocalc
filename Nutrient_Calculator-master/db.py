import sqlite3

# Create a function to connect to the database
def create_connection():
    conn = None
    try:
        conn = sqlite3.connect("my_database.db")
    except sqlite3.Error as e:
        print(e)

    return conn

# Create table users to store the name and email data
conn = create_connection()
conn.execute("CREATE TABLE IF NOT EXISTS users (name TEXT, email TEXT PRIMARY KEY)")
# create a table for the recipes
conn.execute('''
CREATE TABLE IF NOT EXISTS recipes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT NOT NULL,
    name TEXT DEFAULT 'My Meal',
    serving_amounts NUMERIC,
    details TEXT
);
''')
conn.execute('''
CREATE TABLE IF NOT EXISTS log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT NOT NULL,
    name TEXT NOT NULL,
    amounts NUMERIC,
    details TEXT
);
''')

# Remove all recipes from the "recipes" table
# conn.execute("DELETE FROM recipes")
conn.close()

# Save name and email to the table users
def save_user_info(name, email):
    conn = create_connection()
    try:
        # Insert the name and email data into the database
        conn.execute("INSERT OR IGNORE INTO users (name, email) VALUES (?, ?)", (name, email))
        conn.commit()
    except sqlite3.Error as e:
        # Handle any database errors that may occur
        print("An error occurred while saving data to the database:", e)
    finally:
        conn.close()
        
def save_recipe(email, meal_name, serving_amounts, details):
    conn = create_connection()
    try:
        # Insert new recipe
        conn.execute('INSERT INTO recipes (email, name, serving_amounts, details) VALUES (?, ?, ?, ?)', (email, meal_name, serving_amounts, details))
        conn.commit()
    except sqlite3.Error as e:
        # Handle any database errors that may occur
        print("An error occurred while saving data to the database:", e)
    finally:
        conn.close()

def save_log(email, meal_name, serving_amounts, details):
    conn = create_connection()
    try:
        # Insert new recipe
        conn.execute('INSERT INTO log (email, name, amounts, details) VALUES (?, ?, ?, ?)', (email, meal_name, serving_amounts, details))
        conn.commit()

        # # Get the saved data
        # cursor = conn.execute('SELECT id, name, amounts, details FROM log WHERE email = ?', (email,))
        # data = cursor.fetchall()
        # return data        
        
        # # Get the ID of the last inserted row
        # last_row_id = conn.execute('SELECT last_insert_rowid()').fetchone()[0]
        # # Retrieve the last inserted data
        # last_inserted_data = conn.execute('SELECT id, name, amounts, details FROM log WHERE id = ?', (last_row_id,)).fetchone()
        # # Return the last inserted data
        # return last_inserted_data
      
      
    except sqlite3.Error as e:
        # Handle any database errors that may occur
        print("An error occurred while saving data to the database:", e)
    finally:
        conn.close()
        
        
def get_log(email):
    conn = create_connection()
    # retrieve all recipes
    cursor = conn.execute('SELECT id, name, amounts, details FROM log WHERE email = ?', (email,))
    logs = cursor.fetchall()
    conn.close()
    return logs


def update_recipe(recipe_id, meal_name, serving_amounts, details):
    conn = create_connection()
    try:
        # Update recipe
        conn.execute("UPDATE recipes SET name = ?, serving_amounts = ?, details = ? WHERE id = ?", (meal_name, serving_amounts, details, recipe_id))
        conn.commit()
    except sqlite3.Error as e:
        # Handle any database errors that may occur
        print("An error occurred while saving data to the database:", e)
    finally:
        conn.close()

def delete_recipe(recipe_id):
    conn = create_connection()
    try:
        # Delete recipe
        conn.execute("DELETE FROM recipes WHERE id = ?", (recipe_id,))

        conn.commit()
    except sqlite3.Error as e:
        # Handle any database errors that may occur
        print("An error occurred while saving data to the database:", e)
    finally:
        conn.close()

def delete_log(log_id):
    conn = create_connection()
    try:
        # Delete Log
        conn.execute("DELETE FROM log WHERE id = ?", (log_id,))
        
        conn.commit()
    except sqlite3.Error as e:
        # Handle any database errors that may occur
        print("An error occurred while saving data to the database:", e)
    finally:
        conn.close()
    
# Retrieve all name and email data from the database
def get_all_data():
    conn = create_connection()
    cursor = conn.execute("SELECT name, email FROM users")
    data = cursor.fetchall()
    conn.close()
    return data

# Retrieve the name and email data for a given email address
def get_user_info(email):
    conn = create_connection()
    cursor = conn.execute("SELECT name, email FROM users WHERE email = ?", (email,))
    user_info = cursor.fetchone()
    conn.close()
    return user_info
  
# Get all recipe of a user by email address
def get_recipes(email):
    conn = create_connection()
    # retrieve all recipes
    cursor = conn.execute('SELECT id, name, serving_amounts, details FROM recipes WHERE email = ?', (email,))
    recipes = cursor.fetchall()
    conn.close()
    return recipes


# Close the database connection
def close_connection():
    conn.close()
