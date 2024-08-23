package natanel.android.crudapp.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import com.squareup.picasso.Picasso;
import java.util.ArrayList;
import java.util.List;
import jp.wasabeef.picasso.transformations.BlurTransformation;
import natanel.android.crudapp.R;
import natanel.android.crudapp.database.entity.User;
import natanel.android.crudapp.databinding.ItemUserBinding;
import natanel.android.crudapp.service.model.Data;

public class UserAdapter extends RecyclerView.Adapter<UserAdapter.UserViewHolder> {

    private List<User> users = new ArrayList<>();

    public void setUsers(List<User> users) {
        this.users = users;
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public UserViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        LayoutInflater inflater = LayoutInflater.from(parent.getContext());
        ItemUserBinding binding = ItemUserBinding.inflate(inflater, parent, false);
        return new UserViewHolder(binding);
    }

    @Override
    public int getItemCount() {
        return users.size();
    }

    @Override
    public void onBindViewHolder(@NonNull UserViewHolder holder, int position) {
        User user = users.get(position);
        holder.bind(user);
    }

    static class UserViewHolder extends RecyclerView.ViewHolder {

        private final ItemUserBinding binding;

        public UserViewHolder(ItemUserBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }

        public void bind(User user) {
            binding.nameTextView.setText(user.getFirstName() + " " + user.getLastName());
            binding.emailTextView.setText(user.getEmail());
            Picasso.get()
                    .load(user.getAvatar())
                    .placeholder(R.drawable.user_placeholder)
                    .into(binding.avatarImage);

            Picasso.get()
                    .load(user.getAvatar())
                    .transform(new BlurTransformation(itemView.getContext(), 25))
                    .into(binding.avatarBlurImage);
        }
    }
}
