package natanel.android.crudapp.database.repository;

import android.content.Context;

import androidx.lifecycle.LiveData;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import natanel.android.crudapp.database.AppDatabase;
import natanel.android.crudapp.database.dao.UserDao;
import natanel.android.crudapp.database.entity.User;
import natanel.android.crudapp.service.model.Data;

public class UserRepository {
    private final UserDao userDao;
    private final Executor executor = Executors.newSingleThreadExecutor(); // Executor for background tasks

    public UserRepository(Context context) {
        AppDatabase db = AppDatabase.getDatabase(context);
        userDao = db.userDao();
    }

    // Retrieve paginated users from the database
    public LiveData<List<User>> getPaginatedUsers(int pageNumber) {
        int displayItems = 6;
        //int displayItems = PAGE_SIZE;
        int offset = (pageNumber - 1) * displayItems;
        return userDao.getUsers(displayItems, offset);
    }

    // Map Data list to User list and update the database
    public void updateDao(List<Data> dataList) {
        List<User> userList = new ArrayList<>();
        for (Data data : dataList) {
            User user = new User();
            user.setId(data.getId());
            user.setAvatar(data.getAvatar());
            user.setEmail(data.getEmail());
            user.setFirstName(data.getFirstName());
            user.setLastName(data.getLastName());
            user.setPosition(data.getId());
            userList.add(user);
        }

        // Perform the database operation on a background thread
        executor.execute(() -> userDao.insertUsers(userList));
    }

    // Method to generate a new user ID
    public void getUserCount(Callback<Integer> callback) {
        executor.execute(() -> {
            int count = userDao.getUserCount();
            callback.onComplete(count);
        });
    }

    public void addUser(User newUser) {
        // Run operations in a transaction
        executor.execute(() -> {
            userDao.incrementUserPositions();  // Increment positions of existing users
            userDao.insertUser(newUser);  // Insert new user
        });
    }

    public void deleteUser(int userId) {
        executor.execute(() -> userDao.deleteUser(userId));
    }

    public void updateUser(User user) {
        executor.execute(() -> userDao.updateUser(user.getId(), user.getFirstName(), user.getLastName(), user.getEmail(), user.getAvatar()));
    }
}


