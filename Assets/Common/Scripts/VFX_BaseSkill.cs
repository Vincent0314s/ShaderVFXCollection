using System.Collections;
using UnityEngine;
using UnityEngine.Events;
using System;

public class VFX_BaseSkill : MonoBehaviour
{
    public KeyCode testKey = KeyCode.Space;
    public UnityEvent OnStart;
    public float firstTimer = 1.5f;
    public UnityEvent OnSecondEffect;
    public float secondTimer = 5f;
    public UnityEvent OnStop;

    // void Update()
    // {
    //     if (Input.GetKeyDown(testKey)) {
    //         LaunchSkill();
    //     }
    // }

    public void LaunchSkill(Action _cb) {
        StartCoroutine(LaunchSkillCoroutine(_cb));
    }

    IEnumerator LaunchSkillCoroutine(Action _cb = null) {
        OnStart.Invoke();
        var _firstTimer = new WaitForSeconds(this.firstTimer);
        yield return _firstTimer;
        OnSecondEffect.Invoke();
        var _secondTimer = new WaitForSeconds(this.secondTimer);
        yield return _secondTimer;
        OnStop.Invoke();
        _cb?.Invoke();
        Destroy(gameObject,5f);
    }
}
