using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MissileLauncher : MonoBehaviour
{
    public Missile[] PF_missiles;
    public Transform launchPoint;
    private void Update() {
        if(Input.GetKeyDown(KeyCode.M)){
            CreateMissile();
        }
    }

    private void CreateMissile(){
        foreach (var missile in PF_missiles)
        {
            Instantiate(missile,launchPoint.position,launchPoint.rotation);
        }
    }
}
