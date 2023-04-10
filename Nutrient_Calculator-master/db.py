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

# Close the database connection
def close_connection():
    conn.close()
