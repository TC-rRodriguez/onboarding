using System.IO.Abstractions.TestingHelpers;
using System.Text;
using DependencyInversion;

namespace Api.Tests
{
    public class LogBookTest
    {
        [Fact]
        public void AddDescriptionToLogbook()
        {
            // Arrange
            var description = "Test description";
            var filePath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "logbook.txt");
            File.Delete(filePath);
            var logbook = new Logbook();

            // Act
            logbook.Add(description);

            // Assert
            Assert.True(File.Exists(filePath));
            var fileContent = File.ReadAllText(filePath, Encoding.Unicode);
            Assert.Equal($"{description}\n", fileContent);
        }
    }
}