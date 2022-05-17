using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Events;

public class VFX_BaseSkill : MonoBehaviour
{
    public KeyCode testKey = KeyCode.Space;
    public UnityEvent OnStart;
    public float firstTimer = 1.5f;
    public UnityEvent OnSecondEffect;
    public float secondTimer = 5f;
    public UnityEvent OnStop;

    void Update()
    {
        if (Input.GetKeyDown(testKey)) {
            LaunchSkill();
        }
    }

    void LaunchSkill() {
        StartCoroutine(LaunchSkillCoroutine());
    }

    IEnumerator LaunchSkillCoroutine() {
        OnStart.Invoke();
        var _firstTimer = new WaitForSeconds(this.firstTimer);
        yield return _firstTimer;
        OnSecondEffect.Invoke();
        var _secondTimer = new WaitForSeconds(this.secondTimer);
        yield return _secondTimer;
        OnStop.Invoke();
    }
}
