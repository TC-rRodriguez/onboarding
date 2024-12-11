using System.Collections.ObjectModel;

namespace SingleResponsability;

/// <summary>
/// A fake storage class that simulates a data storage mechanism using an ObservableCollection.
/// </summary>
/// <typeparam name="T">The type of elements stored in the collection.</typeparam>
public class FakeStorage<T>
{
    private ObservableCollection<T> _collection;

    /// <summary>
    /// Initializes a new instance of the <see cref="FakeStorage{T}"/> class.
    /// </summary>
    public FakeStorage()
    {
        _collection = new ObservableCollection<T>();
    }

    /// <summary>
    /// Adds an item to the storage.
    /// </summary>
    /// <param name="item">The item to add.</param>
    /// <returns>The added item.</returns>
    public T Add(T item)
    {
        _collection.Add(item);
        return item;
    }

    /// <summary>
    /// Removes an item from the storage.
    /// </summary>
    /// <param name="item">The item to remove.</param>
    /// <returns>The removed item.</returns>
    public T Remove(T item)
    {
        _collection.Remove(item);
        return item;
    }

    /// <summary>
    /// Gets all items from the storage.
    /// </summary>
    /// <returns>An enumerable collection of all items.</returns>
    public IEnumerable<T> GetAll()
    {
        return _collection;
    }
}