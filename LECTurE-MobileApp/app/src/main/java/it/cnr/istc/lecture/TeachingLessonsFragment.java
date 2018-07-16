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

public class TeachingLessonsFragment extends Fragment {

    private RecyclerView teaching_lessons_recycler_view;
    private RecyclerView.Adapter teaching_lessons_adapter;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_teaching_lessons, container, false);

        teaching_lessons_recycler_view = view.findViewById(R.id.teaching_lessons_recycler_view);
        teaching_lessons_adapter = new TeachingLessonsAdapter();

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        teaching_lessons_recycler_view.setHasFixedSize(true);
        teaching_lessons_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        teaching_lessons_recycler_view.setAdapter(teaching_lessons_adapter);

        return view;
    }

    private static class TeachingLessonsAdapter extends RecyclerView.Adapter<TeachingLessonView> {

        private final List<TeachingLessonContext> lessons = new ArrayList<>();

        @NonNull
        @Override
        public TeachingLessonView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new TeachingLessonView(LayoutInflater.from(parent.getContext()).inflate(R.layout.teaching_lesson_row, parent));
        }

        @Override
        public void onBindViewHolder(@NonNull TeachingLessonView holder, int position) {
            TeachingLessonContext lesson = lessons.get(position);
            holder.title.setText(lesson.getLesson().name);
        }

        @Override
        public int getItemCount() {
            return lessons.size();
        }
    }

    private static class TeachingLessonView extends RecyclerView.ViewHolder {

        public TextView title;

        public TeachingLessonView(View view) {
            super(view);
            title = view.findViewById(R.id.teaching_lesson_name);
        }
    }
}
