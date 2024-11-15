using UnityEngine;

public class PlayerDialogTrigger : MonoBehaviour
{

    private bool CanStartDialog = false;
    private Yarn.Unity.DialogueRunner dialog_runner;
    private Yarn.Unity.LineView line_view;
    private string currentTag;
    
    // Start is called once before the first execution of Update after the MonoBehaviour is created
    void Start()
    {
        
    }

    private void Awake()
    {
        dialog_runner = GameObject.Find("Dialogue System").GetComponent<Yarn.Unity.DialogueRunner>();
        line_view = GameObject.Find("Line View").GetComponent<Yarn.Unity.LineView>();
    }

    // Update is called once per frame
    void Update()
    {
        if(CanStartDialog)
        {
            if(Input.GetKeyDown(KeyCode.E))
            {
                if(dialog_runner != null)
                {
                    if(currentTag == "Stranger")
                    {
                        dialog_runner.StartDialogue("yapperMain");
                    }
                    if (currentTag == "NPCTime")
                    {
                        dialog_runner.StartDialogue("NPCTime");
                    }
                    if (currentTag == "NPCChange")
                    {
                        dialog_runner.StartDialogue("NPCChange");
                    }
                    if (currentTag == "NPCHarvest")
                    {
                        dialog_runner.StartDialogue("NPCHarvest");
                    }
                    if (currentTag == "NPCDecay")
                    {
                        dialog_runner.StartDialogue("NPCDecay");
                    }
                }

                if(line_view != null)
                {
                    line_view.UserRequestedViewAdvancement();
                }
            }

            
        }
    }

    private void OnTriggerEnter(Collider other)
    {
        if (other.tag == "Stranger" || other.tag == "NPCTime" || other.tag == "NPCChange" || other.tag == "NPCHarvest" || other.tag == "NPCDecay")
        {
           currentTag = other.tag;
           CanStartDialog = true;
           print("hit npc");
        }
    }

    private void OnTriggerExit(Collider other)
    {
        if (other.tag == "Stranger" || other.tag == "NPCTime" || other.tag == "NPCChange" || other.tag == "NPCHarvest" || other.tag == "NPCDecay")
        {
            currentTag = "nothing";
            CanStartDialog = false;
            print("exited npc");
        }
    }

}
