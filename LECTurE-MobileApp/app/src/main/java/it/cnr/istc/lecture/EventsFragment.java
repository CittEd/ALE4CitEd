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

import it.cnr.istc.lecture.api.messages.Event;
import it.cnr.istc.lecture.api.messages.QuestionEvent;
import it.cnr.istc.lecture.api.messages.TextEvent;
import it.cnr.istc.lecture.api.messages.URLEvent;

public class EventsFragment extends Fragment {

    private RecyclerView events_recycler_view;
    private RecyclerView.Adapter events_adapter;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_learn, container, false);

        events_recycler_view = view.findViewById(R.id.events_recycler_view);
        events_adapter = new EventAdapter();

        // use this setting to improve performance if you know that changes
        // in content do not change the layout size of the RecyclerView
        events_recycler_view.setHasFixedSize(true);
        events_recycler_view.setLayoutManager(new LinearLayoutManager(getContext()));
        events_recycler_view.setAdapter(events_adapter);

        return view;
    }

    private static class EventAdapter extends RecyclerView.Adapter<EventView> {

        private final List<Event> events = new ArrayList<>();

        @NonNull
        @Override
        public EventView onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            return new EventView(LayoutInflater.from(parent.getContext()).inflate(R.layout.event_row, parent));
        }

        @Override
        public void onBindViewHolder(@NonNull EventView holder, int position) {
            Event event = events.get(position);
            switch (event.event_type) {
                case TextEvent:
                    holder.title.setText(((TextEvent) event).content);
                    break;
                case QuestionEvent:
                    holder.title.setText(((QuestionEvent) event).question);
                    break;
                case URLEvent:
                    holder.title.setText(((URLEvent) event).content);
                    break;
            }
        }

        @Override
        public int getItemCount() {
            return events.size();
        }
    }

    private static class EventView extends RecyclerView.ViewHolder {

        public TextView title;

        public EventView(View view) {
            super(view);
            title = view.findViewById(R.id.event_title);
        }
    }
}
