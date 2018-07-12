package it.cnr.istc.lecture;

import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;

public class NavigatorActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Intent intent;

        SharedPreferences shared_prefs = PreferenceManager.getDefaultSharedPreferences(this);
        if (shared_prefs.contains(getString(R.string.email)) && shared_prefs.contains(getString(R.string.password)) && LECTurEContext.getInstance().login(this, shared_prefs.getString(getString(R.string.email), null), shared_prefs.getString(getString(R.string.password), null))) {
            intent = new Intent(this, MainActivity.class);
        } else {
            intent = new Intent(this, LoginActivity.class);
        }

        startActivity(intent);
        finish();
    }
}
