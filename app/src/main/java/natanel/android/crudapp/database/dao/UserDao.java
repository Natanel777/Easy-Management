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

    @Insert
    void insertUser(User user);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertUsers(List<User> users);

    @Query("SELECT * FROM users ORDER BY position ASC LIMIT :limit OFFSET :offset")
    LiveData<List<User>> getUsers(int limit, int offset);

    @Query("UPDATE users SET position = position + 1")
    void incrementUserPositions();

    @Query("SELECT COUNT(*) FROM users")
    int getUserCount();

    @Query("DELETE FROM users WHERE id = :userId")
    void deleteUser(int userId);

    @Query("UPDATE users SET firstName = :firstName, lastName = :lastName, email = :email, avatar = :avatar WHERE id = :userId")
    void updateUser(int userId, String firstName, String lastName, String email, String avatar);
}
