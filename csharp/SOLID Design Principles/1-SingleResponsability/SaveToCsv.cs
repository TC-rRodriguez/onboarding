using System.Text;

namespace SingleResponsability;

/// <summary>
/// Provides functionality to save data to a CSV file.
/// </summary>
public static class SaveToCsv
{
    /// <summary>
    /// Saves the specified data to a CSV file with the given file name.
    /// </summary>
    /// <param name="data">The data to save.</param>
    /// <param name="fileName">The name of the file (without extension).</param>
    public static void Save(string data, string fileName)
    {
        var fileOutput = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, $"{fileName}.csv");
        File.WriteAllText(fileOutput, data, Encoding.Unicode);
    }
}