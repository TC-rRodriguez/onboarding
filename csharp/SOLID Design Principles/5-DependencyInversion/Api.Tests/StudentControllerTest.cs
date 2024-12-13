using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;
using Api.Interfaces;
using DependencyInversion;
using DependencyInversion.Controllers;

namespace Api.Tests
{
    public class StudentControllerTest
    {
        [Fact]
        public async Task Get_ReturnsAllStudents()
        {
            // Arrange
            var mockStudentService = new Mock<IStudentService>();
            var students = new List<Student>
            {
                new Student(1, "John Doe", new List<double> { 3.5, 4.0 }),
                new Student(2, "Jane Doe", new List<double> { 3.0, 3.5 })
            };
            mockStudentService.Setup(service => service.GetAllStudents()).ReturnsAsync(students);
            var controller = new StudentController(mockStudentService.Object);

            // Act
            var result = await controller.Get();

            // Assert
            Assert.Equal(students, result);
        }

        [Fact]
        public async Task Add_AddsStudent()
        {
            // Arrange
            var mockStudentService = new Mock<IStudentService>();
            var newStudent = new Student(3, "New Student", new List<double> { 3.5, 4.0 });
            var controller = new StudentController(mockStudentService.Object);

            // Act
            await controller.Add(newStudent);

            // Assert
            mockStudentService.Verify(service => service.AddStudent(newStudent), Times.Once);
        }
    }
}