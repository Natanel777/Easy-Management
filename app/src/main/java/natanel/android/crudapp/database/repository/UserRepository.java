package natanel.android.crudapp.database.repository;

import static android.nfc.tech.MifareUltralight.PAGE_SIZE;

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
        int offset = (pageNumber - 1) * displayItems ;
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
            userList.add(user);
        }

        // Perform the database operation on a background thread
        executor.execute(() -> userDao.insertUsers(userList));
    }

    public void delete(){
        userDao.deleteAllUsers();
    }
}
