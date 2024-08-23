plugins {
    alias(libs.plugins.androidApplication)
}

android {
    namespace = "natanel.android.crudapp"
    compileSdk = 34

    viewBinding{
        enable = true
    }

    defaultConfig {
        applicationId = "natanel.android.crudapp"
        minSdk = 24
        targetSdk = 34
        versionCode = 1
        versionName = "1.0"

        testInstrumentationRunner = "androidx.test.runner.AndroidJUnitRunner"
    }

    buildTypes {
        release {
            isMinifyEnabled = false
            proguardFiles(
                getDefaultProguardFile("proguard-android-optimize.txt"),
                "proguard-rules.pro"
            )
        }
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_1_8
        targetCompatibility = JavaVersion.VERSION_1_8
    }
}

dependencies {

    // Your other existing dependencies
    implementation(libs.appcompat)
    implementation(libs.material)
    implementation(libs.activity)
    implementation(libs.constraintlayout)
    implementation(libs.navigation.fragment)
    implementation(libs.legacy.support.v4)
    testImplementation(libs.junit)
    androidTestImplementation(libs.ext.junit)
    androidTestImplementation(libs.espresso.core)

    // AndroidX Lifecycle components for Java
    implementation("androidx.lifecycle:lifecycle-viewmodel:2.3.1")
    implementation("androidx.lifecycle:lifecycle-runtime:2.2.0")

    // For circle image XML
    implementation("de.hdodenhof:circleimageview:3.1.0")

    // Retrofit
    implementation("com.squareup.retrofit2:retrofit:2.9.0")
    implementation("com.squareup.retrofit2:converter-gson:2.9.0")
    implementation("com.squareup.okhttp3:okhttp:4.9.3")

    //LiveData
    implementation("androidx.lifecycle:lifecycle-livedata:2.2.0")

    //Picasso for image loading
    implementation("com.squareup.picasso:picasso:2.8")

    // Blur picasso image
    implementation("jp.wasabeef:picasso-transformations:2.4.0")

    // Room
    implementation("androidx.room:room-runtime:2.2.6")
    annotationProcessor("androidx.room:room-compiler:2.2.6")


}