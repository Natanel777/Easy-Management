package natanel.android.crudapp.database.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;

import java.util.List;

import natanel.android.crudapp.database.entity.User;

@Dao
public interface UserDao {

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertUsers(List<User> users);

    @Query("SELECT * FROM users LIMIT :limit OFFSET :offset")
    LiveData<List<User>> getUsers(int limit, int offset);

    @Query("SELECT COUNT(*) FROM users")
    int getUserCount();

    @Query("DELETE FROM users")
    void deleteAllUsers();
}
