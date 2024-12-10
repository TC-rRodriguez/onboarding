namespace ToDo;

internal static class Program
{
    private static List<string>? TaskList { get; set; }
    
    private const int AddTaskOption = 1;
    private const int RemoveTaskOption = 2;
    private const int ShowTaskListOption = 3;
    private const int ExitOption = 4;

    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    /// <param name="args">An array of command-line argument strings.</param>
    /// <remarks>
    /// Initializes the task list and displays the main menu in a loop until the user chooses to exit.
    /// Based on the user's menu selection, it calls the appropriate method to add, remove, or show tasks.
    /// </remarks>
    private static void Main(string[] args)
    {
        TaskList = new List<string>();
        int menuOption;
        do
        {
            menuOption = ShowMainMenu();

            switch (menuOption)
            {
                case AddTaskOption: AddTask();
                    break;
                case RemoveTaskOption: RemoveTask();
                    break;
                case ShowTaskListOption: ShowTaskList();
                    break;
                case ExitOption:
                    Console.WriteLine("Bye!");
                    break;
                default:
                    Console.WriteLine("Invalid option");
                    break;
            }
        } while (menuOption != 4);
    }

    /// <summary>
    /// Displays the list of tasks.
    /// </summary>
    /// <remarks>
    /// If there are no tasks in the list, a message indicating that there are no tasks to perform is displayed.
    /// Otherwise, each task is displayed with its corresponding index.
    /// </remarks>
    private static void ShowTaskList()
    {
        if (TaskList == null || TaskList.Count == 0)
        {
            Console.WriteLine("There are no tasks to perform");
            return;
        }
            
        var index = 1;
        foreach (var task in TaskList)
        {
            Console.WriteLine(index + ". " + task);
            index++;
        }
    }
        
    /// <summary>
    /// Displays the main menu and reads the user's option.
    /// </summary>
    /// <returns>
    /// Returns the option indicated by the user as an integer. 
    /// If the input is invalid, returns 0.
    /// </returns>
    private static int ShowMainMenu()
    {
        Console.WriteLine("----------------------------------------");
        Console.WriteLine("Enter the option to perform: ");
        Console.WriteLine("1. New task");
        Console.WriteLine("2. Remove task");
        Console.WriteLine("3. Pending tasks");
        Console.WriteLine("4. Exit");

        // Read line
        var line = Console.ReadLine();
            
        return int.TryParse(line, out var option) ? option : 0;
    }

    /// <summary>
    /// Removes a task from the TaskList based on user input.
    /// </summary>
    /// <remarks>
    /// Displays the current list of tasks and prompts the user to enter the number of the task to remove.
    /// If the input is valid, the task is removed from the list and a confirmation message is displayed.
    /// If the input is invalid or an error occurs, an appropriate message is displayed.
    /// </remarks>
    /// <exception cref="Exception">Thrown when an error occurs during the task removal process.</exception>
    private static void RemoveTask()
    {
        try
        {
            if (TaskList == null || TaskList.Count == 0)
            {
                Console.WriteLine("There are no tasks to remove");
                return;
            }
                
            ShowTaskList();
            Console.WriteLine("----------------------------------------");
            Console.WriteLine("Enter the number of the task to remove: ");
                
            var line = Console.ReadLine();
            var taskIndex = int.TryParse(line, out var index) ? index : 0;
                
            if (taskIndex == 0 || taskIndex > TaskList.Count)
            {
                Console.WriteLine("Invalid task number");
                return;
            }
                
            Console.WriteLine("Task " + TaskList[taskIndex - 1] + " deleted");
            TaskList.RemoveAt(taskIndex - 1);
        }
        catch (Exception)
        {
            Console.WriteLine("An error occurred while removing the task");
            throw;
        }
    }

    /// <summary>
    /// Displays the menu to add a new task and handles the task addition process.
    /// </summary>
    /// <remarks>
    /// Prompts the user to enter the name of the task. If the task name is valid (not null or empty),
    /// it adds the task to the TaskList and confirms the addition. If an error occurs during the process,
    /// an error message is displayed.
    /// </remarks>
    /// <exception cref="Exception">Thrown when an error occurs while adding the task.</exception>
    private static void AddTask()
    {
        try
        {
            Console.WriteLine("Enter the name of the task: ");
            var task = Console.ReadLine();
                
            if (string.IsNullOrEmpty(task))
            {
                Console.WriteLine("Task name cannot be empty");
                return;
            }

            TaskList.Add(task);
                
            Console.WriteLine("Task registered successfully");
        }
        catch (Exception)
        {
            Console.WriteLine("An error occurred while adding the task");
        }
    }
}
