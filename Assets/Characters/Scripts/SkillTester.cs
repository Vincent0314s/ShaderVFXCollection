using System.Collections;
using System.Collections.Generic;
using UnityEngine;


[System.Serializable]
public struct SkillContainer{
    public enum TriggerAnimation{
        None,
        Beginning,
        First,
        Second
    }
    public VFX_BaseSkill PF_Skill;
    public TriggerAnimation cancelTrigger;    
}

public class SkillTester : MonoBehaviour
{
    public SkillContainer[] skillToTest;
    private int skillIndex;
    [SerializeField]
    private Transform skillPoint;
    private Animator anim;
    private bool isCastingSkill;

    void Awake(){
        anim = transform.GetChild(0).GetComponent<Animator>();

    }
    void Start()
    {
        skillIndex = 0;
    }

    void Update()
    {
        if(Input.GetKeyDown(KeyCode.Space)){
           CreateSkillVFX();
        }
        if(Input.GetKeyDown(KeyCode.Q) && skillIndex > 0){
            skillIndex -= 1;
        }
         if(Input.GetKeyDown(KeyCode.E) && skillIndex < skillToTest.Length - 1){
            skillIndex += 1;
        }
        anim.SetInteger("SkillIndex",skillIndex);
        anim.SetBool("isCasting",isCastingSkill);
    }
    
    void CreateSkillVFX(){
        isCastingSkill = true;

        VFX_BaseSkill currentVFX = Instantiate(skillToTest[skillIndex].PF_Skill,skillPoint.position,Quaternion.identity);
        switch (skillToTest[skillIndex].cancelTrigger)
        {
            case SkillContainer.TriggerAnimation.First:
            currentVFX.LaunchSkill(() => isCastingSkill = false);
            break;
            case SkillContainer.TriggerAnimation.Second:
            currentVFX.LaunchSkill(null,() => isCastingSkill = false);
            break;
        }
    }
}
