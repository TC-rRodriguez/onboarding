using Microsoft.AspNetCore.Mvc.Testing;
using System.Text.Json;
using Microsoft.VisualStudio.TestPlatform.TestHost;

namespace DependencyInversion.IntegrationTests
{
    public class StudentControllerIntegrationTests : IClassFixture<WebApplicationFactory<Program>>
    {
        private readonly HttpClient _client;

        public StudentControllerIntegrationTests(WebApplicationFactory<Program> factory)
        {
            _client = factory.CreateClient();
        }

        [Fact]
        public async Task Get_ReturnsStudentsList()
        {
            // Act
            var response = await _client.GetAsync("/student");
            response.EnsureSuccessStatusCode();

            var responseString = await response.Content.ReadAsStringAsync();
            var students = JsonSerializer.Deserialize<List<Student>>(responseString, new JsonSerializerOptions { PropertyNameCaseInsensitive = true });

            // Assert
            Assert.NotNull(students);
            Assert.Equal(3, students.Count);
        }

        [Fact]
        public async Task Add_AddsStudent()
        {
            // Arrange
            var newStudent = new Student { Id = 4, Fullname = "New Student", Grades = new List<double> { 3.5, 4.0 } };
            var content = new StringContent(JsonSerializer.Serialize(newStudent), System.Text.Encoding.UTF8, "application/json");

            // Act
            var response = await _client.PostAsync("/student", content);
            response.EnsureSuccessStatusCode();

            // Assert
            var getResponse = await _client.GetAsync("/student");
            getResponse.EnsureSuccessStatusCode();

            var responseString = await getResponse.Content.ReadAsStringAsync();
            var students = JsonSerializer.Deserialize<List<Student>>(responseString, new JsonSerializerOptions { PropertyNameCaseInsensitive = true });

            Assert.NotNull(students);
            Assert.Contains(students, s => s.Fullname == "New Student");
        }
    }
}