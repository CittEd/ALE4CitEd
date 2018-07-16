package it.cnr.istc.lecture;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.List;

public class FollowingLessonsFragment extends Fragment {

    private RecyclerView following_lessons_recycler_view;
    private RecyclerView.Adapter following_lessons_adapter;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_following_lessons, container, false);

        following_lessons_recycler_view = view.findViewById(R.id.following_lessons_recycler_view);
        following_lessons_adapter = new FollowingLessonsAdapter();

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        following_lessons_recycler_view.setHasFixedSize(true);
        following_lessons_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        following_lessons_recycler_view.setAdapter(following_lessons_adapter);

        return view;
    }

    private static class FollowingLessonsAdapter extends RecyclerView.Adapter<FollowingLessonView> {

        private final List<FollowingLessonContext> lessons = new ArrayList<>();

        @NonNull
        @Override
        public FollowingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new FollowingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.following_lesson_row, parent));
        }

        @Override
        public void onBindViewHolder(@NonNull FollowingLessonView holder, int position) {
            FollowingLessonContext lesson = lessons.get(position);
            holder.title.setText(lesson.getLesson().name);
        }

        @Override
        public int getItemCount() {
            return lessons.size();
        }
    }

    private static class FollowingLessonView extends RecyclerView.ViewHolder {

        public TextView title;

        public FollowingLessonView(View view) {
            super(view);
            title = view.findViewById(R.id.following_lesson_name);
        }
    }
}
